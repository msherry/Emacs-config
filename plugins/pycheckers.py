#!/usr/bin/env python
"""A hacked up version of the multiple-Python checkers script from EmacsWiki.

 - Simplified & faster
 - Extended with pep8.py
 - Extended with pydo (http://www.lunaryorn.de/code/pydo.html)
 - pylint & pychecker removed

Drop something like this in your .emacs:

(when (load "flymake" t)
  (defun flymake-pycheckers-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/path/to/this/file" (list local-file))))


You may also need to set up your path up in the __main__ function at the
bottom of the file and change the #! line above to an appropriate interpreter.

==============================================================================

This code is made available by Jason Kirtland <jek@discorporate.us> under the
Creative Commons Share Alike 1.0 license:
http://creativecommons.org/licenses/sa/1.0/

Original work taken from http://www.emacswiki.org/emacs/PythonMode, author
unknown.

"""
## Customization ##

# Checkers to run be default, when no --checkers options are supplied.
# One or more of pydo, pep8 or pyflakes, separated by commas
# default_checkers = 'pep8, pyflakes'
default_checkers = 'pylint,pep8'

# A list of error codes to ignore for PEP8
# default_ignore_codes = ['E225', 'W114']
default_ignore_codes = \
    [
    # 'E202',                       # Whitespace before ']'
    # 'E221',                       # Multiple spaces before operator
    # 'E225',                       # Missing whitespace around operator
    # 'E231',                       # Missing whitespace after ':'
    # 'E241',                       # Multiple spaces after ':'
    # 'E261',                       # At least two spaces before inline comment
    # 'W291',                       # Trailing whitespace
    # 'E301',                       # Expected 1 blank line, found 0
    # 'E302',                       # Expected 2 blank lines, found 1
    # 'E303',                       # Too many blank lines
    # 'E401',                       # Multiple imports on one line
    # 'E501',                       # Line too long

    # 'E127',                # continuation line over-indented for visual indent
    # 'E128',                # continuation line under-indented for visual indent
    'E711',                # comparison to None should be...
    'E712',                # comparison to True/False should be ...
    ]

## End of customization ##

import os
from os import path
import re
import sys


from subprocess import Popen, PIPE


class LintRunner(object):
    """Base class provides common functionality to run python code checkers."""

    output_format = ("%(level)s %(error_type)s%(error_number)s:"
                     "%(description)s at %(filename)s line %(line_number)s.")

    output_template = dict.fromkeys(
        ('level', 'error_type', 'error_number', 'description',
         'filename', 'line_number'), '')

    output_matcher = None

    sane_default_ignore_codes = set([])

    command = None

    run_flags = ()

    def __init__(self, ignore_codes=(), use_sane_defaults=True):
        self.ignore_codes = set(ignore_codes)
        if use_sane_defaults:
            self.ignore_codes ^= self.sane_default_ignore_codes

    def fixup_data(self, line, data):
        return data

    def process_output(self, line):
        m = self.output_matcher.match(line)
        if m:
            return m.groupdict()

    def run(self, filename):
        args = [self.command]
        args.extend(self.run_flags)
        args.append(filename)

        errors_or_warnings = 0

        try:
            process = Popen(args, stdout=PIPE, stderr=PIPE)
        except Exception, e:
            print e, args
            return

        for line in process.stdout:
            match = self.process_output(line)
            if match:
                tokens = dict(self.output_template)
                tokens.update(self.fixup_data(line, match))
                print self.output_format % tokens
                errors_or_warnings += 1

        for line in process.stderr:
            match = self.process_output(line)
            if match:
                tokens = dict(self.output_template)
                tokens.update(self.fixup_data(line, match))
                print self.output_format % tokens
                errors_or_warnings += 1

        return errors_or_warnings


class PyflakesRunner(LintRunner):
    """Run pyflakes, producing flymake readable output.

    The raw output looks like:
      tests/test_richtypes.py:4: 'doom' imported but unused
      tests/test_richtypes.py:33: undefined name 'undefined'
    or:
      tests/test_richtypes.py:40: could not compile
             deth
            ^
    """

    command = 'pyflakes'

    output_matcher = re.compile(
        r'(?P<filename>[^:]+):'
        r'(?P<line_number>[^:]+):'
        r'(?P<description>.+)$')

    @classmethod
    def fixup_data(cls, line, data):
        if 'imported but unused' in data['description']:
            data['level'] = 'WARNING'
        elif 'redefinition of unused' in data['description']:
            data['level'] = 'WARNING'
        elif 'assigned to but never used' in data['description']:
            data['level'] = 'WARNING'
        elif 'unable to detect undefined names' in data['description']:
            data['level'] = 'WARNING'
        else:
            data['level'] = 'ERROR'
        data['error_type'] = 'PY'
        data['error_number'] = 'F'

        return data


class Pep8Runner(LintRunner):
    """Run pep8.py, producing flymake readable output.

    The raw output looks like:
      spiders/structs.py:3:80: E501 line too long (80 characters)
      spiders/structs.py:7:1: W291 trailing whitespace
      spiders/structs.py:25:33: W602 deprecated form of raising exception
      spiders/structs.py:51:9: E301 expected 1 blank line, found 0

    """

    command = 'pep8'

    output_matcher = re.compile(
        r'(?P<filename>[^:]+):'
        r'(?P<line_number>[^:]+):'
        r'[^:]+:'
        r' (?P<error_number>\w+) '
        r'(?P<description>.+)$')

    @classmethod
    def fixup_data(cls, line, data):
        data['level'] = 'WARNING'
        return data

    @property
    def run_flags(self):
        return '--repeat', '--ignore=' + ','.join(self.ignore_codes)


class PydoRunner(LintRunner):
    """Run pydo, producing flymake readable output.

    The raw output looks like:
      users.py:356:FIXME this will fail if None
      users.py:470:todo:rtf memcache this possibly?
      users.py:482:TODO This will need to trigger a history entry and

    """

    command = 'pydo'

    output_matcher = re.compile(
        r'(?P<filename>[^:]+):'
        r'(?P<line_number>[^:]+):'
        r'(?P<error_number>\w+)'
        r'(\W*|\s*)'
        r'(?P<description>.*)$')

    @classmethod
    def fixup_data(cls, line, data):
        number = data['error_number'] = data['error_number'].upper()
        if number == 'FIXME':
            data['level'] = 'ERROR'
        else:
            data['level'] = 'WARNING'
        return data


class PylintRunner(LintRunner):
    """ Run pylint, producing flymake readable output.

    The raw output looks like:
    render.py:49: [C0301] Line too long (82/80)
    render.py:1: [C0111] Missing docstring
    render.py:3: [E0611] No name 'Response' in module 'werkzeug'
    render.py:32: [C0111, render] Missing docstring """

    command = 'pylint'

    output_matcher = re.compile(
        r'(?P<filename>[^:]+):'
        r'(?P<line_number>\d+):'
        r'\s*\[(?P<error_type>[WECR])(?P<error_number>[^(,\]]+),?'
        r'\s*(?P<context>[^\]]*)\]'
        r'\s*(?P<description>.*)$')

    sane_default_ignore_codes = set([
        "C0103",  # Naming convention
        "C0111",  # Missing Docstring
        "W0142",
        "W0201",  # "Attribute defined outside __init__"
        "W0232",  # No __init__
        "W0403",
        "W0511",
        "E1101",
        "E1103",  # Instance of x has no y member
                  # (but some types could not be inferred")
        "R0201",  # Method could be a function
        "R0801",  # Similar lines in * files
        "R0903",  # Too few public methods
        "R0904",  # Too many public methods
    ])

    @classmethod
    def fixup_data(cls, line, data):
        if data['error_type'].startswith('E'):
            data['level'] = 'ERROR'
        else:
            data['level'] = 'WARNING'
        return data

    @property
    def run_flags(self):
        return ('--output-format', 'parseable',
                '--reports', 'n',
                '--disable=' + ','.join(self.sane_default_ignore_codes),
                '--dummy-variables-rgx=' + '_.*'
                )


def croak(*msgs):
    for m in msgs:
        print >> sys.stderr, m.strip()
    sys.exit(1)


RUNNERS = {
    'pyflakes': PyflakesRunner,
    'pep8': Pep8Runner,
    'pydo': PydoRunner,
    'pylint': PylintRunner
}


if __name__ == '__main__':
    # transparently add a virtualenv to the path when launched with a venv'd
    # python.
    os.environ['PATH'] = (path.dirname(sys.executable) + ':' +
                          os.environ['PATH'])

    if len(sys.argv) < 2:
        croak("Usage: %s [file]" % sys.argv[0])
    elif len(sys.argv) > 2:
        from optparse import OptionParser
        parser = OptionParser()
        parser.add_option("-i", "--ignore_codes", dest="ignore_codes",
                          default=[], action='append',
                          help="error codes to ignore")
        parser.add_option("-c", "--checkers", dest="checkers",
                          default=default_checkers,
                          help="comma separated list of checkers")
        options, args = parser.parse_args()
        if not args:
            croak("Usage: %s [file]" % sys.argv[0])
        if options.checkers:
            checkers = options.checkers
        else:
            checkers = default_checkers
        if options.ignore_codes:
            ignore_codes = options.ignore_codes
        else:
            ignore_codes = default_ignore_codes
        source_file = args[0]
    else:
        source_file = sys.argv[1]
        checkers = default_checkers
        ignore_codes = default_ignore_codes

    # Attempt to determine if the current file is in a virtualenv, and munge
    # paths appropriately
    # TODO: this is very pp-specific
    full_path = os.path.abspath(source_file)
    if '/pp/' in full_path:
        package = re.search(r'/pp/([^/]+)', full_path).group(1)
        virtualenv_path = os.path.expanduser('~{}/.virtualenvs/{}'.format(
            os.environ['USER'], package))
        if os.path.exists(virtualenv_path):
            bin_path = os.path.join(virtualenv_path, 'bin')
            os.environ['PATH'] = bin_path + ':' + os.environ['PATH']

    errors_or_warnings = 0
    for checker in checkers.split(','):
        try:
            klass = RUNNERS[checker.strip()]
        except KeyError:
            croak(("Unknown checker %s" % checker),
                  ("Expected one of %s" % ', '.join(RUNNERS.keys())))
            break
        runner = klass(ignore_codes=ignore_codes)
        errors_or_warnings += runner.run(source_file)

    exit_status = 0
    if errors_or_warnings > 0:
        exit_status = 1
    sys.exit(exit_status)
