# -*- coding:utf-8 -*-

#
#   This file contains routines to change names of corpus files
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this file. If not, see <http://www.gnu.org/licenses/>.
#
#   Copyright 2013-2014 Børre Gaup <borre.gaup@uit.no>
#

import os
import sys
import subprocess
import platform
import argparse
import shutil

import lxml.etree as etree
import unidecode
import requests

import argparse_version
import versioncontrol
import xslsetter


class NameChangerBase(object):
    """Class to change names of corpus files.
    Will also take care of changing info in meta data of parallel files.
    """

    def __init__(self, oldname):
        """Find the directory the oldname is in.
        self.oldname is the basename of oldname.
        self.newname is the basename of oldname, in lowercase and
        with some characters replaced.
        """
        self.old_filename = os.path.basename(oldname)
        self.old_dirname = os.path.dirname(oldname)

        self.new_filename = self.change_to_ascii()

    def change_to_ascii(self):
        """Downcase all chars in self.oldname, replace some chars
        """
        unwanted_chars = {
            u'+': '_', u' ': u'_', u'(': u'_', u')': u'_', u"'": u'_',
            u'–': u'-', u'?': u'_', u',': u'_', u'!': u'_', u',': u'_',
            u'<': u'_', u'>': u'_', u'"': u'_', u'&': u'_', u';': u'_',
            u'&': u'_'}

        # unidecode.unidecode makes ascii only
        newname = unicode(unidecode.unidecode(
            self.old_filename)).lower()

        for key, value in unwanted_chars.items():
            if key in newname:
                newname = newname.replace(key, value)

        while '__' in newname:
            newname = newname.replace('__', '_')

        return newname


class AddFileToCorpus(NameChangerBase):
    '''Add a given file to a given corpus
    '''
    def __init__(self, oldname, corpusdir, mainlang, path):
        super(AddFileToCorpus, self).__init__(oldname)
        self.mainlang = mainlang
        self.path = path
        self.new_dirname = os.path.join(corpusdir, 'orig', mainlang, path)
        vcsfactory = versioncontrol.VersionControlFactory()
        self.vcs = vcsfactory.vcs(corpusdir)

    def makedirs(self):
        try:
            os.makedirs(self.new_dirname)
            self.vcs.add_directory(self.new_dirname)
        except OSError:
            pass

    def add_url_extension(self, content_type):
        content_type_extension = {
            'text/html': '.html',
            'application/msword': '.doc',
            'application/pdf': '.pdf',
            'text/plain': '.txt',
        }

        for ct, extension in content_type_extension.items():
            if (ct in content_type and not
                    self.new_filename.endswith(extension)):
                self.new_filename += extension

    def toname(self):
        return os.path.join(self.new_dirname, self.new_filename)

    def copy_orig_to_corpus(self):
        '''Copy the original file to the correct place in the given corpus
        '''
        fromname = os.path.join(self.old_dirname, self.old_filename)
        self.makedirs()

        if fromname.startswith('http'):
            try:
                r = requests.get(fromname)
                if r.status_code == requests.codes.ok:
                    self.add_url_extension(r.headers['content-type'])
                    if not os.path.exists(self.toname()):
                        with open(self.toname(), 'wb') as new_corpus_file:
                            new_corpus_file.write(r.content)
                        print self.toname()
                        self.vcs.add(self.toname())
                    else:
                        print >>sys.stderr, self.toname(), 'already exists'
                else:
                    print >>sys.stderr
                    print >>sys.stderr, 'ERROR:', fromname, 'does not exist'
                    print >>sys.stderr
                    raise UserWarning
            except requests.exceptions.MissingSchema:
                print >>sys.stderr, 'Error: wrong schema'
                raise UserWarning
            except requests.exceptions.ConnectionError as e:
                print >>sys.stderr, 'Could not connect'
                print >>sys.stderr, str(e)
                raise UserWarning
        else:
            shutil.copy(fromname, self.toname())
            print self.toname()
            self.vcs.add(self.toname())

    def make_metadata_file(self, extra_values):
        '''extra_values is a dict that contains data for the metadata file
        that is not possible to infer from the data given in the constructor.
        '''
        metafile_name = self.toname() + '.xsl'
        if not os.path.exists(metafile_name):
            metadata_file = xslsetter.MetadataHandler(metafile_name, create=True)
            if self.old_dirname.startswith('http'):
                metadata_file.set_variable('filename', os.path.join(
                    self.old_dirname, self.old_filename))
            else:
                metadata_file.set_variable('filename', self.old_filename)
            metadata_file.set_variable('genre', self.path.split('/')[0])
            metadata_file.set_variable('mainlang', self.mainlang)
            metadata_file.set_variable('sub_name',
                                       self.vcs.user_name().decode('utf-8'))
            metadata_file.set_variable('sub_email', self.vcs.user_email())

            for key, value in extra_values.items():
                metadata_file.set_variable(key, value)

            print metadata_file.filename
            metadata_file.write_file()
            self.vcs.add(metadata_file.filename)
        else:
            print >>sys.stderr, metafile_name, 'already exists'


class CorpusNameFixer(NameChangerBase):
    def __init__(self, oldname):
        super(CorpusNameFixer, self).__init__(oldname)

    def change_name(self):
        """Change the name of the original file and it's metadata file
        Update the name in parallel files
        Also move the other files that's connected to the original file
        """
        if self.old_filename != self.new_filename:
            fullname = os.path.join(self.old_dirname, self.new_filename)
            if not os.path.exists(fullname):
                self.move_origfile()
                self.move_xslfile()
                self.update_name_in_parallel_files()
                self.move_prestable_converted()
                self.move_prestable_toktmx()
                self.move_prestable_tmx()
            else:
                print >>sys.stderr, (
                    '\nError renaming {}'
                    '\n{} exists\n'.format(
                        os.path.join(self.old_dirname, self.old_filename),
                        fullname))

    def move_file(self, oldname, newname):
        """Change name of file from fromname to toname"""
        if not os.path.exists(os.path.dirname(newname)):
            os.makedirs(os.path.dirname(newname))
        if newname != oldname:
            subprocess.call(['git', 'mv', oldname, newname])

    def move_origfile(self):
        """Change the name of the original file
        using the routines of a given repository tool
        """
        fromname = os.path.join(self.old_dirname, self.old_filename)
        toname = os.path.join(self.old_dirname, self.new_filename)

        self.move_file(fromname, toname)

    def move_xslfile(self):
        """Change the name of an xsl file using the
        routines of a given repository tool
        """
        fromname = os.path.join(self.old_dirname,
                                '{}.xsl'.format(self.old_filename))
        toname = os.path.join(self.old_dirname,
                              '{}.xsl'.format(self.new_filename))

        if os.path.exists(fromname):
            self.move_file(fromname, toname)

    def open_xslfile(self, xslfile):
        """Open xslfile, return the tree"""
        try:
            tree = etree.parse(xslfile)
        except Exception, inst:
            print "Unexpected error opening {}: {}".format(xslfile, inst)
            sys.exit(254)

        return tree

    def set_para_backreference(self, mainlang, paralang, paraname):
        """Find the xsl file of the parallel file, and make it point correctly
        to the new name of this file.

        """
        paradir = self.old_dirname.replace("/"+mainlang+"/",
                                           "/"+paralang+"/")
        parafile = os.path.join(paradir, '{}.xsl'.format(paraname))
        if os.path.exists(parafile):
            paradata = xslsetter.MetadataHandler(parafile)
            paradata.set_parallel_text(mainlang, self.new_filename)
            paradata.write_file()

    def update_name_in_parallel_files(self):
        """Open the .xsl file belonging to the file we are changing names of.
        Look for parallel files.
        Open the xsl files of these parallel files and change the name of this
        parallel from the old to the new one
        """
        xslfile = os.path.join(self.old_dirname,
                               '{}.xsl'.format(self.new_filename))
        if os.path.exists(xslfile):
            metadata = xslsetter.MetadataHandler(xslfile)
            xslroot = metadata.tree.getroot()

            mainlang = xslroot.find(".//*[@name='mainlang']").get(
                'select').strip("'")

            if mainlang != "":
                parallels = metadata.get_parallel_texts()
                for paralang, paraname in parallels.iteritems():
                    self.set_para_backreference(mainlang, paralang, paraname)

    def move_prestable_converted(self):
        """Move the file in prestable/converted from the old to the new name
        """
        dirname = self.old_dirname.replace('/orig/', '/prestable/converted/')
        fromname = os.path.join(dirname, '{}.xml'.format(self.old_filename))
        toname = os.path.join(dirname, '{}.xml'.format(self.new_filename))

        if os.path.exists(fromname):
            self.move_file(fromname, toname)

    def move_prestable_toktmx(self):
        """Move the file in prestable/toktmx from the old to the new name
        """
        for suggestion in ['/prestable/toktmx/sme2nob/',
                           '/prestable/toktmx/nob2sme/']:
            dirname = self.old_dirname.replace('/orig/', suggestion)
            fromname = os.path.join(dirname, '{}.toktmx'.format(
                self.old_filename))
            if os.path.exists(fromname):
                toname = os.path.join(dirname, '{}.toktmx'.format(
                    self.new_filename))
                self.move_file(fromname, toname)

    def move_prestable_tmx(self):
        """Move the file in prestable/tmx from the old to the new name
        """
        for suggestion in ['/prestable/tmx/sme2nob/',
                           '/prestable/tmx/nob2sme/']:
            dirname = self.old_dirname.replace('/orig/', suggestion)
            fromname = os.path.join(dirname, '{}.tmx'.format(
                self.old_filename))
            if os.path.exists(fromname):
                toname = os.path.join(dirname, '{}.tmx'.format(
                    self.new_filename))
                self.move_file(fromname, toname)


def name_to_unicode(filename):
    if platform.system() == 'Windows':
        return filename
    else:
        return filename.decode('utf-8')


def parse_args():
    parser = argparse.ArgumentParser(
        parents=[argparse_version.parser],
        description='Program to automatically rename corpus files in the \
        given directory. It downcases the filenames and removes unwanted \
        characters.')

    parser.add_argument('directory',
                        help='The directory where filenames should be \
                        renamed')

    return parser.parse_args()


def main():
    args = parse_args()

    for root, dirs, files in os.walk(args.directory):
        for file_ in files:
            if not file_.endswith('.xsl'):
                nc = CorpusNameFixer(
                    name_to_unicode(os.path.join(root, file_)))
                nc.change_name()

def updater_main():
    args = parse_args()

    for root, dirs, files in os.walk(args.directory):
        for file_ in files:
            if file_.endswith('.xsl'):
                nc = xslsetter.MetadataUpdater(
                    name_to_unicode(os.path.join(root, file_)))
                nc.fix()
                nc.write_file()


def gather_files(origs):
    file_list = []

    for orig in origs:
        if os.path.isdir(orig):
            for root, dirs, files in os.walk(orig):
                for f in files:
                    file_list.append(name_to_unicode(os.path.join(root, f)))
        elif os.path.isfile(orig):
            file_list.append(name_to_unicode(orig))
        elif orig.startswith('http'):
            file_list.append(name_to_unicode(orig))
        else:
            print >>sys.stderr, (
                'ERROR: {} is neither a directory, nor a file nor a '
                'http-url\n'.format(orig))
            raise UserWarning

    return file_list


def add_files(args):
    for orig in gather_files(args.origs):
        adder = AddFileToCorpus(
            orig,
            args.corpusdir,
            args.mainlang,
            args.path)
        adder.copy_orig_to_corpus()
        adder.make_metadata_file({})


def parse_options():
    parser = argparse.ArgumentParser(
        parents=[argparse_version.parser],
        description='Copy files to a corpus directory. The filenames are \
        converted to ascii only names. Metadata files containing the \
        original name, the main language and the genre are also made. The \
        files are added to the working copy.')

    parser.add_argument('corpusdir',
                        help='The corpus dir (freecorpus or boundcorpus)')
    parser.add_argument('mainlang',
                        help='The language of the files that will be added \
                        (sma, sme, ...)')
    parser.add_argument('path',
                        help='The genre directory where the files will be \
                        added. This may also be a path, e.g. \
                        admin/facta/skuvlahistorja1')
    parser.add_argument('origs',
                        nargs='+',
                        help='The original files, urls or directories where the \
                        original files reside (not in svn)')

    return parser.parse_args()


def adder_main():
    args = parse_options()

    if os.path.isdir(args.corpusdir):
        try:
            add_files(args)
        except UserWarning:
            pass
    else:
        print >>sys.stderr, 'ERROR'
        print >>sys.stderr, 'The given corpus directory,',
        print >>sys.stderr, args.corpusdir,
        print >>sys.stderr, 'does not exist'
