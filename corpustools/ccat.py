# -*- coding:utf-8 -*-

#
#   This file contains a class and main function to convert giellatekno xml
#   formatted files to pure text
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this file. If not, see <http://www.gnu.org/licenses/>.
#
#   Copyright 2013-2014 Børre Gaup <borre.gaup@uit.no>
#

from lxml import etree
import StringIO
import os
import sys
import argparse

import argparse_version


class XMLPrinter:
    """This is a class to convert giellatekno xml formatted files to plain text
    """
    def __init__(self,
                 lang=None,
                 all_paragraphs=False,
                 title=False,
                 listitem=False,
                 table=False,
                 correction=False,
                 error=False,
                 errorort=False,
                 errorortreal=False,
                 errormorphsyn=False,
                 errorsyn=False,
                 errorlex=False,
                 errorlang=False,
                 foreign=False,
                 noforeign=False,
                 typos=False,
                 print_filename=False,
                 one_word_per_line=False,
                 disambiguation=False,
                 dependency=False,
                 hyph_replacement=''):
        '''The handling of error* elements are governed by the error*,
        noforeign, correction, typos and one_word_per_line arguments.

        If one_word_per_line and typos are False and correction is True, the
        content of the correct attribute should be printed instead of the
        .text part of the error element.

        If one_word_per_line or typos are True, the .text part, the correct
        attribute and the other attributes of the error* element should be
        printed out on one line.

        If typos is True and some of the error* options are True, only the
        elements that are True should be output

        If one_word_per_line is True and some of the error* options are True,
        only the elements that are True should get the error treatment, the
        other ones get treated as plain elements.

        If noforeign is True, neither the errorlang.text part nor the correct
        attribute should be printed.
        '''

        self.paragraph = True
        self.all_paragraphs = all_paragraphs

        if title or listitem or table:
            self.paragraph = False

        self.title = title
        self.listitem = listitem
        self.table = table

        self.correction = correction
        self.error = error
        self.errorort = errorort
        self.errorortreal = errorortreal
        self.errormorphsyn = errormorphsyn
        self.errorsyn = errorsyn
        self.errorlex = errorlex
        self.errorlang = errorlang
        self.noforeign = noforeign

        if (error or
                errorort or
                errorortreal or
                errormorphsyn or
                errorsyn or
                errorlex or
                errorlang):
            self.error_filtering = True
        else:
            self.error_filtering = False

        self.typos = typos
        self.print_filename = print_filename
        if self.typos:
            self.one_word_per_line = True
        else:
            self.one_word_per_line = one_word_per_line

        if lang and lang.startswith('!'):
            self.lang = lang[1:]
            self.invert_lang = True
        else:
            self.lang = lang
            self.invert_lang = False

        self.disambiguation = disambiguation
        self.dependency = dependency

        if hyph_replacement == 'xml':
            self.hyph_replacement = '<hyph/>'
        else:
            self.hyph_replacement = hyph_replacement

    def get_lang(self):
        """
        Get the lang of the file
        """
        return self.etree.getroot().\
            attrib['{http://www.w3.org/XML/1998/namespace}lang']

    def get_element_language(self, element, parentlang):
        """Get the language of element.

        Elements inherit the parents language if not explicitely set
        """
        if element.get('{http://www.w3.org/XML/1998/namespace}lang') is None:
            return parentlang
        else:
            return element.get('{http://www.w3.org/XML/1998/namespace}lang')

    def collect_not_inline_errors(self, element, textlist):
        '''Add the formatted errors as strings to the textlist list
        '''
        error_string = self.error_not_inline(element)
        if error_string != '':
            textlist.append(error_string)

        for child in element:
            if self.visit_error_not_inline(child):
                self.collect_not_inline_errors(child, textlist)

        if not self.typos:
            if element.tail is not None and element.tail.strip() != '':
                if not self.one_word_per_line:
                    textlist.append(element.tail.strip())
                else:
                    textlist.append('\n'.join(element.tail.strip().split()))

    def error_not_inline(self, element):
        '''Collect and format element.text, element.tail and
        the attributes into the string text

        Also scan the children if there is no error filtering or
        if the element is filtered
        '''
        text = ''
        if element.text is not None and element.text.strip() != '':
            text = element.text.strip()

        if not self.error_filtering or self.include_this_error(element):
            for child in element:
                if text != '':
                    text += ' '
                if child.tag == 'span' and element.tag == 'errorsyn':
                    text += child.text
                else:
                    try:
                        text += child.get('correct')
                    except TypeError:
                        print >>sys.stderr, 'Unexpected error element'
                        print >>sys.stderr, etree.tostring(child,
                                                           encoding='utf8')
                        print >>sys.stderr, 'To fix this error you must \
                        fix the errormarkup in the original document:'
                        print >>sys.stderr, self.filename

                if child.tail is not None and child.tail.strip() != '':
                    text += u' {}'.format(child.tail.strip())

        text += self.get_error_attributes(dict(element.attrib))

        return text

    def get_error_attributes(self, attributes):
        '''Collect and format the attributes + the filename
        into the string text.
        '''
        text = '\t'
        text += attributes.get('correct')
        del attributes['correct']

        attr = []
        for key in sorted(attributes):
            attr.append(u'{}={}'.format(key,
                                        unicode(attributes[key])))

        if len(attr) > 0:
            text += '\t#'
            text += ','.join(attr)

            if self.print_filename:
                text += u', file: {}'.format(
                    os.path.basename(self.filename).decode('utf8'))

        elif self.print_filename:
            text += u'\t#file: {}'.format(
                os.path.basename(self.filename).decode('utf8'))

        return text

    def collect_inline_errors(self, element, textlist, parentlang):
        '''Add the "correct" element to the list textlist
        '''
        if element.get('correct') is not None and not self.noforeign:
            textlist.append(element.get('correct'))

        self.get_tail(element, textlist, parentlang)

    def collect_text(self, element, parentlang, buffer):
        """Collect text from element, and write the contents to buffer
        """
        textlist = []

        self.visit_nonerror_element(element, textlist, parentlang)

        if len(textlist) > 0:
            if not self.one_word_per_line:
                buffer.write(' '.join(textlist).encode('utf8'))
                buffer.write(' ¶\n')
            else:
                buffer.write('\n'.join(textlist).encode('utf8'))
                buffer.write('\n')

    def get_contents(self, elt_contents, textlist, elt_lang):
        if elt_contents is not None:
            text = elt_contents.strip()
            if text != '' and (
                    self.lang is None or
                    (not self.invert_lang and elt_lang == self.lang) or
                    (self.invert_lang and elt_lang != self.lang)):
                if not self.one_word_per_line:
                    textlist.append(text)
                else:
                    textlist.append('\n'.join(text.split()))

    def get_text(self, element, textlist, parentlang):
        '''Get the text part of an lxml element
        '''
        self.get_contents(element.text,
                          textlist,
                          self.get_element_language(element, parentlang))

    def get_tail(self, element, textlist, parentlang):
        '''Get the tail part of an lxml element
        '''
        self.get_contents(element.tail,
                          textlist,
                          parentlang)

    def visit_children(self, element, textlist, parentlang):
        """Visit the children of element, adding their content to textlist
        """
        for child in element:
            if child.tag == 'errorlang' and self.noforeign and self.typos:
                pass
            elif child.tag == 'errorlang' and self.noforeign:
                self.get_tail(child, textlist, parentlang)
            elif self.visit_error_inline(child):
                self.collect_inline_errors(
                    child,
                    textlist,
                    self.get_element_language(child, parentlang))
            elif self.visit_error_not_inline(child):
                self.collect_not_inline_errors(child, textlist)
            else:
                self.visit_nonerror_element(
                    child,
                    textlist,
                    self.get_element_language(element, parentlang))

    def visit_nonerror_element(self, element, textlist, parentlang):
        """Visit and extract text from non error element
        """
        if not self.typos:
            self.get_text(element, textlist, parentlang)
        self.visit_children(element, textlist, parentlang)
        if not self.typos:
            self.get_tail(element, textlist, parentlang)

    def visit_this_node(self, element):
        '''Return True if the element should be visited
        '''
        return (
            self.all_paragraphs or
            (
                self.paragraph is True and (element.get('type') is None or
                                            element.get('type') == 'text')
            ) or (
                self.title is True and element.get('type') == 'title'
            ) or (
                self.listitem is True and element.get('type') == 'listitem'
            ) or (
                self.table is True and element.get('type') == 'tablecell'
            )
        )

    def visit_error_not_inline(self, element):
        """Determine whether element should be visited
        """
        return (
            element.tag.startswith('error') and self.one_word_per_line and not
            self.error_filtering or
            self.include_this_error(element)
            )

    def visit_error_inline(self, element):
        """Determine whether element should be visited
        """
        return (element.tag.startswith('error') and not
                self.one_word_per_line and
                (self.correction or self.include_this_error(element))
                )

    def include_this_error(self, element):
        """Determine whether element should be visited
        """
        return self.error_filtering and (
            (element.tag == 'error' and self.error) or
            (element.tag == 'errorort' and self.errorort) or
            (element.tag == 'errorortreal' and self.errorortreal) or
            (element.tag == 'errormorphsyn' and self.errormorphsyn) or
            (element.tag == 'errorsyn' and self.errorsyn) or
            (element.tag == 'errorlex' and self.errorlex) or
            (element.tag == 'errorlang' and self.errorlang) or
            (element.tag == 'errorlang' and self.noforeign)
            )

    def parse_file(self, filename):
        self.filename = filename
        p = etree.XMLParser(huge_tree=True)
        self.etree = etree.parse(filename, p)

    def process_file(self):
        """Process the given file, adding the text into buffer

        Returns the buffer
        """
        buffer = StringIO.StringIO()

        if self.hyph_replacement is not None:
            self.handle_hyph()
        if self.dependency:
            self.print_element(self.etree.find('.//dependency'), buffer)
        elif self.disambiguation:
            self.print_element(self.etree.find('.//disambiguation'), buffer)
        else:
            for paragraph in self.etree.findall('.//p'):
                if self.visit_this_node(paragraph):
                    self.collect_text(paragraph, self.get_lang(), buffer)

        return buffer

    def handle_hyph(self):
        """Replace hyph tags
        """
        hyph_tails = []
        for hyph in self.etree.findall('.//hyph'):
            if hyph.tail is not None:
                hyph_tails.append(hyph.tail)

            if hyph.getnext() is None:
                if hyph.getparent().text is not None:
                    hyph_tails.insert(0, hyph.getparent().text)
                hyph.getparent().text = self.hyph_replacement.join(hyph_tails)
                hyph_tails[:] = []

            hyph.getparent().remove(hyph)

    def print_element(self, element, buffer):
        if element is not None and element.text is not None:
            buffer.write(element.text.encode('utf8'))

    def print_file(self, file_):
        '''Print a xml file to stdout'''
        if file_.endswith('.xml'):
            self.parse_file(file_)
            sys.stdout.write(self.process_file().getvalue())


def parse_options():
    """Parse the options given to the program
    """
    parser = argparse.ArgumentParser(
        parents=[argparse_version.parser],
        description='Print the contents of a corpus in XML format\n\
        The default is to print paragraphs with no type (=text type).')

    parser.add_argument('-l',
                        dest='lang',
                        help='Print only elements in language LANG. Default \
                        is all langs.')
    parser.add_argument('-T',
                        dest='title',
                        action='store_true',
                        help='Print paragraphs with title type', )
    parser.add_argument('-L',
                        dest='list',
                        action='store_true',
                        help='Print paragraphs with list type')
    parser.add_argument('-t',
                        dest='table',
                        action='store_true',
                        help='Print paragraphs with table type')
    parser.add_argument('-a',
                        dest='all_paragraphs',
                        action='store_true',
                        help='Print all text elements')

    parser.add_argument('-c',
                        dest='corrections',
                        action='store_true',
                        help='Print corrected text instead of the original \
                        typos & errors')
    parser.add_argument('-C',
                        dest='error',
                        action='store_true',
                        help='Only print unclassified (§/<error..>) \
                        corrections')
    parser.add_argument('-ort',
                        dest='errorort',
                        action='store_true',
                        help='Only print ortoghraphic, non-word \
                        ($/<errorort..>) corrections')
    parser.add_argument('-ortreal',
                        dest='errorortreal',
                        action='store_true',
                        help='Only print ortoghraphic, real-word \
                        (¢/<errorortreal..>) corrections')
    parser.add_argument('-morphsyn',
                        dest='errormorphsyn',
                        action='store_true',
                        help='Only print morphosyntactic \
                        (£/<errormorphsyn..>) corrections')
    parser.add_argument('-syn',
                        dest='errorsyn',
                        action='store_true',
                        help='Only print syntactic (¥/<errorsyn..>) \
                        corrections')
    parser.add_argument('-lex',
                        dest='errorlex',
                        action='store_true',
                        help='Only print lexical (€/<errorlex..>) \
                        corrections')
    parser.add_argument('-foreign',
                        dest='errorlang',
                        action='store_true',
                        help='Only print foreign (∞/<errorlang..>) \
                        corrections')
    parser.add_argument('-noforeign',
                        dest='noforeign',
                        action='store_true',
                        help='Do not print anything from foreign \
                        (∞/<errorlang..>) corrections')
    parser.add_argument('-typos',
                        dest='typos',
                        action='store_true',
                        help='Print only the errors/typos in the text, with \
                        corrections tab-separated')
    parser.add_argument('-f',
                        dest='print_filename',
                        action='store_true',
                        help='Add the source filename as a comment after each \
                        error word.')
    parser.add_argument('-S',
                        dest='one_word_per_line',
                        action='store_true',
                        help='Print the whole text one word per line; \
                        typos have tab separated corrections')
    parser.add_argument('-dis',
                        dest='disambiguation',
                        action='store_true',
                        help='Print the disambiguation element')
    parser.add_argument('-dep',
                        dest='dependency',
                        action='store_true',
                        help='Print the dependency element')
    parser.add_argument('-hyph',
                        dest='hyph_replacement',
                        default='',
                        help='Replace hyph tags with the given argument')

    parser.add_argument('targets',
                        nargs='+',
                        help='Name of the files or directories to process. \
                        If a directory is given, all files in this directory \
                        and its subdirectories will be listed.')

    args = parser.parse_args()
    return args


def main():
    """Set up the XMLPrinter class with the given command line options and
    process the given files and directories
    Print the output to stdout
    """
    args = parse_options()

    xml_printer = XMLPrinter(lang=args.lang,
                             all_paragraphs=args.all_paragraphs,
                             title=args.title,
                             listitem=args.list,
                             table=args.table,
                             correction=args.corrections,
                             error=args.error,
                             errorort=args.errorort,
                             errorortreal=args.errorortreal,
                             errormorphsyn=args.errormorphsyn,
                             errorsyn=args.errorsyn,
                             errorlex=args.errorlex,
                             errorlang=args.errorlang,
                             noforeign=args.noforeign,
                             typos=args.typos,
                             print_filename=args.print_filename,
                             one_word_per_line=args.one_word_per_line,
                             dependency=args.dependency,
                             disambiguation=args.disambiguation,
                             hyph_replacement=args.hyph_replacement)

    for target in args.targets:
        if os.path.exists(target):
            if os.path.isfile(target):
                xml_printer.print_file(target)
            elif os.path.isdir(target):
                for root, _, files in os.walk(target):
                    for xml_file in files:
                        xml_printer.print_file(os.path.join(root, xml_file))
        else:
            print >>sys.stderr, '{} does not exist'.format(target)

if __name__ == '__main__':
    main()
