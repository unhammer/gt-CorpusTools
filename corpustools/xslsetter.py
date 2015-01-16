# -*- coding:utf-8 -*-

from __future__ import unicode_literals

import os
import sys

import lxml.etree as etree

import util

here = os.path.dirname(__file__)


class MetadataHandler(object):
    '''Class to handle metadata in .xsl files
    '''

    lang_key = "{http://www.w3.org/XML/1998/namespace}lang"
    namespaces = { 'xsl': 'http://www.w3.org/1999/XSL/Transform',
                   'xml': 'http://www.w3.org/XML/1998/namespace',
                   're': 'http://exslt.org/regular-expressions' }

    def __init__(self, filename, create=False):
        self.filename = filename

        if not os.path.exists(filename):
            if not create:
                raise util.ArgumentError("{} does not exist!".format(filename))
            preprocessXsl = etree.parse(os.path.join(here,
                                                     'xslt/preprocxsl.xsl'))
            preprocessXslTransformer = etree.XSLT(preprocessXsl)
            filexsl = etree.parse(os.path.join(here, 'xslt/XSL-template.xsl'))
            self.tree = preprocessXslTransformer(
                filexsl,
                commonxsl=etree.XSLT.strparam(
                    'file://' + os.path.join(here, 'xslt/common.xsl')))
        else:
            self.tree = etree.parse(filename)

    def _get_variable_elt(self, key):
        return self.tree.getroot().find(
            "{{http://www.w3.org/1999/XSL/Transform}}"
            "variable[@name='{}']".format(key))

    def set_variable(self, key, value):
        try:
            variable = self._get_variable_elt(key)
            variable.attrib['select'] = "'{}'".format(value)
        except AttributeError as e:
            print >>sys.stderr, (
                'Tried to update {} with value {}\n'
                'Error was {}'.format(key, value, str(e))).encode('utf-8')
            raise UserWarning

    def get_variable(self, key):
        variable = self._get_variable_elt(key)
        return variable.attrib['select'].replace("'", "")


    def get_parallel_texts(self):
        parallels = self._get_variable_elt("parallels")
        if parallels is None:
            return {}
        else:
            elts = parallels.findall("parallel_text")
            return { p.attrib[self.lang_key]: p.attrib["location"].strip("'")
                     for p in elts
                     if p.attrib["location"].strip("'") != "" }

    def set_parallel_text(self, language, location):
        attrib = { self.lang_key: language,
                   "location" : location }
        parallels = self._get_variable_elt("parallels")
        if parallels is None:
            parallels = etree.Element("{http://www.w3.org/1999/XSL/Transform}variable",
                                      name="parallels")
            parallels.text, parallels.tail = "\n", "\n\n"
            self.tree.getroot().append(parallels)
        elt = parallels.find("parallel_text[@{}='{}']".format(self.lang_key, language))
        if elt is not None:
            elt.attrib.update(attrib)
        else:
            elt = etree.Element("parallel_text", attrib=attrib)
            elt.tail = "\n"
            parallels.append(elt)


    def write_file(self):
        try:
            self.tree.write(self.filename, encoding="utf-8",
                            xml_declaration=True)
        except IOError:
            print 'cannot write', self.filename
            sys.exit(254)


class MetadataUpdater(MetadataHandler):
    """Provisional class, to fix documents that still have the old xsl
    """
    def _fix_hardcoded_lang_elts(self, toggle, prefix, settings_to_elt):
        root = self.tree.getroot()
        toggle_elt = root.xpath("xsl:variable[@name='{}']".format(toggle),
                                namespaces=self.namespaces)
        if toggle_elt:
            map(root.remove, toggle_elt)
        old = root.xpath("xsl:variable[starts-with(@name,'{}')]".format(prefix),
                         namespaces=self.namespaces)
        if old:
            settings = [(e.attrib["name"].replace(prefix, ""),
                         e.attrib["select"].strip("'"))
                        for e in old
                        if e.attrib.get("select", "''") != "''"]
            new, comment = settings_to_elt(settings)
            new.text, new.tail = "\n\t", "\n\n"
            for i,c in enumerate(new):
                if i==len(new)-1:
                    c.tail = "\n"
                else:
                    c.tail = "\n\t"
            i_old = root.index(old[0])
            root[i_old-1].tail += "\n\n"
            comment.tail = "\n"
            root.insert(i_old, new)
            root.insert(i_old, comment)
            map(root.remove, old)

    def fix_comments(self):
        bad = set([
            'lg rec is off!',
            " If the document is multilingual, set the variable multilingual to\n     '1' (and possibly change mlangs below). ",
            'this is default',
            ' Select the potential langugages by adding the number "1" in the ',
            ' selection frame. If no languages are selected, the document is ',
            ' checked for all supported languages. ',
            ' If the document has parallel texts, select "1" for parallel_texts ',
            ' Tag the specified elements with the specified language: ',
        ])
        for c in self.tree.getroot().iter(etree.Comment):
            if c.text in bad:
                self.tree.getroot().remove(c)

    def fix_mlangs(self):
        def settings_to_elt(settings):
            children = [etree.Element("language",
                                      attrib={self.lang_key: l})
                        for l, v in settings]
            outer = etree.Element("{http://www.w3.org/1999/XSL/Transform}variable",
                                  attrib={ "name": "mlangs" })
            map(outer.append, children)
            return outer, etree.Comment(""" If monolingual is not set, the language is multilingual.
     Uncomment the languages you want to check for (or add new lines
     with the right ISO-639-3 language codes).

     If *no* languages are uncommented (and monolingual is not 1),
     then the document is checked for all supported languages.
""")

        self._fix_hardcoded_lang_elts("multilingual", "mlang_", settings_to_elt)

        old = self.tree.getroot().xpath("xsl:variable[re:test(@name, '^...lang$')]",
                                        namespaces=self.namespaces)
        map(self.tree.getroot().remove, old)

    def fix_para(self):
        def settings_to_elt(settings):
            children = [etree.Element("parallel_text",
                                      attrib={self.lang_key: l, "location": v})
                        for l, v in settings]
            outer = etree.Element("{http://www.w3.org/1999/XSL/Transform}variable",
                                  attrib={ "name": "parallels" })
            map(outer.append, children)
            return outer, etree.Comment(""" If the document has parallel texts, uncomment the right languages
     (or add new lines with the right ISO-639-3 language codes) and
     add the filename of the parallel files to the 'location'
     variables.

     Don't write the full directory; we expect the file to be in the
     same directory as this file, with only the language code and
     filename changed.
     """)

        self._fix_hardcoded_lang_elts("parallel_texts", "para_", settings_to_elt)

    def fix(self):
        self.fix_comments()
        self.fix_mlangs()
        self.fix_para()
