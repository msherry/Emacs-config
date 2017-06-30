#!/usr/local/bin/python
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


==============================================================================

Further modified and extended by Marc Sherry.
"""

from argparse import ArgumentParser
import ConfigParser
from functools import partial
import os
import re
from subprocess import Popen, PIPE
import sys


# Customization #

# Checkers to run by default, when no --checkers options are supplied.
default_checkers = 'flake8,pylint,mypy,mypy3'

# A list of error codes to ignore for PEP8
default_ignore_codes = [
    # 'E202',          # Whitespace before ']'
    # 'E221',          # Multiple spaces before operator
    # 'E225',          # Missing whitespace around operator
    # 'E231',          # Missing whitespace after ':'
    # 'E241',          # Multiple spaces after ':'
    # 'E261',          # At least two spaces before inline comment
    # 'W291',          # Trailing whitespace
    # 'E301',          # Expected 1 blank line, found 0
    # 'E302',          # Expected 2 blank lines, found 1
    # 'E303',          # Too many blank lines
    # 'E401',          # Multiple imports on one line
    # 'E501',          # Line too long

    # 'E127',          # continuation line over-indented for visual indent
    # 'E128',          # continuation line under-indented for visual indent
    'E711',            # comparison to None should be...
    'E712',            # comparison to True/False should be ...
]

# End of customization #


class LintRunner(object):
    """Base class provides common functionality to run python code checkers."""

    output_format = ("%(level)s %(error_type)s%(error_number)s:"
                     "%(description)s at %(filename)s line %(line_number)s.")

    output_template = dict.fromkeys(
        ('level', 'error_type', 'error_number', 'description',
         'filename', 'line_number'), '')

    output_matcher = re.compile(r'')

    sane_default_ignore_codes = set()

    command = ''

    run_flags = ()

    def __init__(self, ignore_codes=(), use_sane_defaults=True, options=None):
        self.ignore_codes = set(ignore_codes)
        if use_sane_defaults:
            self.ignore_codes |= self.sane_default_ignore_codes
        self.options = options
        self.out_lines = []

    @property
    def name(self):
        """The linter's name, which is usually the same as the command.

        They may be different if there are multiple versions run with
        flags -- e.g. the MyPy2Runner's name may be 'mypy2', even though
        the command is just 'mypy'.
        """
        return self.command

    def fixup_data(self, _line, data):
        return data

    def process_output(self, line):
        m = self.output_matcher.match(line)
        if m:
            return m.groupdict()

    def _process_stream(self, stream):
        # This runs over both stdout and stderr
        errors_or_warnings = 0
        for line in stream:
            match = self.process_output(line)
            if match:
                tokens = dict(self.output_template)
                # Return None from fixup_data to ignore this error
                fixed_up = self.fixup_data(line, match)
                if fixed_up:
                    # Prepend the command name to the description (if present)
                    # so we know which checker threw which error
                    if 'description' in fixed_up:
                        fixed_up['description'] = '%s: %s' % (
                            self.name, fixed_up['description'])
                    tokens.update(fixed_up)
                    self.out_lines.append(self.output_format % tokens)
                    errors_or_warnings += 1
        return errors_or_warnings

    def run(self, filename):
        # `env` to use a virtualenv, if found
        args = ['/usr/bin/env', self.command]
        args.extend(self.run_flags)
        args.append(filename)

        try:
            process = Popen(args, stdout=PIPE, stderr=PIPE)
        except Exception as e:
            print e, args
            return

        errors_or_warnings = (self._process_stream(process.stdout) +
                              self._process_stream(process.stderr))

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
    def fixup_data(cls, _line, data):
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


class Flake8Runner(LintRunner):
    """Flake8 has similar output to Pyflakes
    """

    command = 'flake8'

    output_matcher = re.compile(
        r'(?P<filename>[^:]+):'
        '(?P<line_number>[^:]+):'
        '(?P<column_number>[^:]+): '
        '(?P<error_type>[WEFCN])(?P<error_number>[^ ]+) '
        '(?P<description>.+)$')

    @classmethod
    def fixup_data(cls, _line, data):
        if data['error_type'] in ['E']:
            data['level'] = 'WARNING'
        elif data['error_type'] in ['F']:
            data['level'] = 'ERROR'
        else:
            data['level'] = 'WARNING'

        # Unlike pyflakes, flake8 has an error/warning distinction, but some of
        # them are incorrect. Borrow the correct definitions from the pyflakes
        # runner
        if 'imported but unused' in data['description']:
            data['level'] = 'WARNING'
        elif 'redefinition of unused' in data['description']:
            data['level'] = 'WARNING'
        elif 'assigned to but never used' in data['description']:
            data['level'] = 'WARNING'
        elif 'unable to detect undefined names' in data['description']:
            data['level'] = 'WARNING'

        return data

    @property
    def run_flags(self):
        return (
            '--ignore=' + ','.join(self.ignore_codes),
            '--max-line-length', str(self.options.max_line_length),
        )


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
    def fixup_data(cls, _line, data):
        data['level'] = 'WARNING'
        return data

    @property
    def run_flags(self):
        return (
            '--repeat',
            '--ignore=' + ','.join(self.ignore_codes),
            '--max-line-length', str(self.options.max_line_length),
        )


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
    def fixup_data(cls, _line, data):
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
        "E1002",  # Use super on old-style class
        "E1101",
        "E1103",  # Instance of x has no y member
                  # (but some types could not be inferred")
        "R0201",  # Method could be a function
        "R0801",  # Similar lines in * files
        "R0903",  # Too few public methods
        "R0904",  # Too many public methods
        "R0914",  # Too many local variables
    ])

    @classmethod
    def fixup_data(cls, _line, data):
        if data['error_type'].startswith('E'):
            data['level'] = 'ERROR'
        else:
            data['level'] = 'WARNING'
        return data

    @property
    def run_flags(self):
        return (
            '--output-format', 'parseable',
            '--reports', 'n',
            '--disable=' + ','.join(self.ignore_codes),
            '--dummy-variables-rgx=' + '_.*',
            '--max-line-length', str(self.options.max_line_length),
        )


class MyPy2Runner(LintRunner):

    command = 'mypy'

    output_matcher = re.compile(
        r'(?P<filename>[^:]+):'
        r'(?P<line_number>[^:]+):'
        r' (?P<level>[^:]+):'
        r' (?P<description>.+)$')

    @property
    def run_flags(self):
        return (
            '--py2',
            '--ignore-missing-imports',
        )

    def fixup_data(self, _line, data):
        data['level'] = data['level'].upper()
        if data['level'] == 'NOTE':
            return None
        return data


class MyPy3Runner(MyPy2Runner):

    @property
    def name(self):
        return 'mypy3'

    @property
    def run_flags(self):
        return (
            '--ignore-missing-imports',
        )


def croak(*msgs):
    for m in msgs:
        print >> sys.stderr, m.strip()
    sys.exit(1)


RUNNERS = {
    'pyflakes': PyflakesRunner,
    'flake8': Flake8Runner,
    'pep8': Pep8Runner,
    'pydo': PydoRunner,
    'pylint': PylintRunner,
    'mypy': MyPy2Runner,
    'mypy3': MyPy3Runner,
}


def update_options_from_file(options, config_file_path):
    config = ConfigParser.SafeConfigParser()
    config.read(config_file_path)

    for key, value in config.defaults().iteritems():
        if value in ['False', 'false', 'F', 'f']:
            value = False
        elif value in ['True', 'true', 'T', 't']:
            value = True
        setattr(options, key, value)
    for section_name in config.sections():
        if (re.search(section_name, options.file) or
                re.search(section_name, options.file.replace('_flymake', ''))):
            for key, value in config.items(section_name):
                if value in ['False', 'false', 'F', 'f']:
                    value = False
                elif value in ['True', 'true', 'T', 't']:
                    value = True
                setattr(options, key, value)
    if hasattr(options, 'extra_ignore_codes'):
        extra_ignore_codes = (options
                              .extra_ignore_codes.replace(',', ' ').split())
        # Allow for extending, rather than replacing, ignore codes
        options.ignore_codes.extend(extra_ignore_codes)
    return options


def update_options_locally(options):
    """
    Traverse the project directory until a config file is found or the
    filesystem root is reached. If found, use overrides from config as
    project-specific settings.
    """
    dir_path = os.path.dirname(os.path.abspath(options.file))
    config_file_path = os.path.join(dir_path, '.pycheckers')
    while True:
        if os.path.exists(config_file_path):
            options = update_options_from_file(options, config_file_path)
            if not options.merge_configs:
                # We found a file and parsed it, now we're done
                break
        if os.path.dirname(dir_path) == dir_path:
            break
        dir_path = os.path.dirname(dir_path)
        config_file_path = os.path.join(dir_path, '.pycheckers')

    return options


def run_one_checker(ignore_codes, options, source_file, checker_name):
    checker_class = RUNNERS[checker_name]
    runner = checker_class(ignore_codes=ignore_codes, options=options)
    errors_or_warnings = runner.run(source_file)
    return (errors_or_warnings, runner.out_lines)


def update_env_with_virtualenv(source_file):
    """Determine if the current file is part of a package that has a
    virtualenv, and munge paths appropriately"""
    # TODO: this is very unix-specific
    full_path = os.path.abspath(source_file)
    dir_components = os.path.dirname(full_path).split('/')
    # TODO: this should be a setting
    virtualenv_base = os.path.expanduser('~/.virtualenvs/')
    for component in dir_components:
        if not component:
            continue
        virtualenv_path = os.path.join(virtualenv_base, component)
        if os.path.exists(virtualenv_path):
            bin_path = os.path.join(virtualenv_path, 'bin')
            os.environ['PATH'] = bin_path + ':' + os.environ['PATH']
            break


def parse_args():
    parser = ArgumentParser()
    parser.add_argument('file', type=str, help='Filename to check')
    parser.add_argument("-c", "--checkers", dest="checkers",
                        default=default_checkers,
                        help="Comma-separated list of checkers")
    parser.add_argument("-i", "--ignore_codes", dest="ignore_codes",
                        default=default_ignore_codes, action='append',
                        help="Error codes to ignore")
    parser.add_argument('--max-line-length', dest='max_line_length',
                        default=80, action='store',
                        help='Maximum line length')
    parser.add_argument('--no-merge-configs', dest='merge_configs',
                        action='store_false',
                        help=('Whether to ignore config files found at a '
                              'higher directory than this one'))
    return parser.parse_args()


def main():
    # transparently add a virtualenv to the path when launched with a venv'd
    # python.
    os.environ['PATH'] = (os.path.dirname(sys.executable) + ':' +
                          os.environ['PATH'])

    options = parse_args()

    source_file = options.file
    checkers = options.checkers
    ignore_codes = options.ignore_codes

    options = update_options_locally(options)
    update_env_with_virtualenv(source_file)

    checker_names = [checker.strip() for checker in checkers.split(',')]
    try:
        [RUNNERS[checker_name] for checker_name in checker_names]
    except KeyError:
        croak(("Unknown checker %s" % checker_name),
              ("Expected one of %s" % ', '.join(RUNNERS.keys())))

    from multiprocessing import Pool
    p = Pool(5)

    func = partial(run_one_checker, ignore_codes, options, source_file)

    outputs = p.map(func, checker_names)
    p.close()
    p.join()

    counts, out_lines_list = zip(*outputs)
    errors_or_warnings = sum(counts)
    for out_lines in out_lines_list:
        for line in out_lines:
            print line

    sys.exit(errors_or_warnings > 0)


if __name__ == '__main__':
    main()
