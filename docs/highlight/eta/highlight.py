# -*- coding: utf-8 -*-
"""
    eta.highlight
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for Eta.

    :copyright: Copyright 2017 by Rahul Muttineni.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import Lexer, RegexLexer, bygroups, do_insertions, \
    default, include
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Generic
from pygments import unistring as uni

# TODO: Write LiterateEtaLexer
__all__ = ['EtaLexer']

line_re = re.compile('.*?\n')

class EtaLexer(RegexLexer):
    """
    A Eta lexer based on ETA.Parser.Lexer.

    .. versionadded:: 0.8
    """
    name = 'Eta'
    aliases = ['eta']
    filenames = ['*.hs','*.eta']
    mimetypes = ['text/x-eta']

    flags = re.MULTILINE | re.UNICODE

    reserved = ('case', 'class', 'data', 'default', 'deriving', 'do', 'else',
                'if', 'in', 'infix[lr]?', 'instance', 'family',
                'let', 'newtype', 'of', 'then', 'type', 'where', '_')
    ascii = ('NUL', 'SOH', '[SE]TX', 'EOT', 'ENQ', 'ACK',
             'BEL', 'BS', 'HT', 'LF', 'VT', 'FF', 'CR', 'S[OI]', 'DLE',
             'DC[1-4]', 'NAK', 'SYN', 'ETB', 'CAN',
             'EM', 'SUB', 'ESC', '[FGRU]S', 'SP', 'DEL')

    tokens = {
        'root': [
            # Whitespace:
            (r'\s+', Text),
            # (r'--\s*|.*$', Comment.Doc),
            (r'--(?![!#$%&*+./<=>?@^|_~:\\]).*?$', Comment.Single),
            (r'\{-#\s+LANGUAGE', Punctuation),
            (r'\{-#\s+CLASS', Punctuation),
            (r'\#-}', Punctuation),
            (r'\{-', Comment.Multiline, 'comment'),
            # Lexemes:
            #  Identifiers
            (r'foreign\s+(?:import|export)\s+java\s+(?:safe|unsafe|interruptible)', Keyword.Reserved),
            (r'\bimport\b', Keyword.Reserved, 'import'),
            (r'\bmodule\b', Keyword.Reserved, 'module'),
            (r'\berror\b', Name.Exception),
            (r'\b(%s)(?!\')\b' % '|'.join(reserved), Keyword.Reserved),
            (r"'[^\\]'", String.Char),  # this has to come before the TH quote
            (r'^[_' + uni.Ll + r'][\w\']*', Name.Function),
            (r"'?[_" + uni.Ll + r"][\w']*", Name),
            (r"(''|')?[" + uni.Lu + r"][\w\']*", Keyword.Type),
            #  Operators
            (r'\\(?![:!#$%&*+.\\/<=>?@^|~-]+)', Name.Function),  # lambda operator
            (r'(<-|::|->|=>|=)(?![:!#$%&*+.\\/<=>?@^|~-]+)', Operator.Word),  # specials
            (r':[:!#$%&*+.\\/<=>?@^|~-]*', Keyword.Type),  # Constructor operators
            (r'[:!#$%&*+.\\/<=>?@^|~-]+', Operator),  # Other operators
            #  Numbers
            (r'\d+[eE][+-]?\d+', Number.Float),
            (r'\d+\.\d+([eE][+-]?\d+)?', Number.Float),
            (r'0[oO][0-7]+', Number.Oct),
            (r'0[xX][\da-fA-F]+', Number.Hex),
            (r'\d+', Number.Integer),
            (r'\'\[', Punctuation),
            #  Character/String Literals
            (r"'", String.Char, 'character'),
            (r'"', String, 'string'),
            #  Special
            (r'\[\]', Keyword.Type),
            (r'\(\)', Name.Builtin),
            (r'[][(),;`{}]', Punctuation),
        ],
        'import': [
            # Import statements
            (r'\s+', Text),
            (r'"', String, 'string'),
            # after "funclist" state
            (r'\)', Punctuation, '#pop'),
            (r'qualified\b', Keyword),
            # import X as Y
            (r'([' + uni.Lu + r'][\w.]*)(\s+)(as)(\s+)([' + uni.Lu + r'][\w.]*)',
             bygroups(Name.Namespace, Text, Keyword, Text, Name), '#pop'),
            # import X hiding (functions)
            (r'([' + uni.Lu + r'][\w.]*)(\s+)(hiding)(\s+)(\()',
             bygroups(Name.Namespace, Text, Keyword, Text, Punctuation), 'funclist'),
            # import X (functions)
            (r'([' + uni.Lu + r'][\w.]*)(\s+)(\()',
             bygroups(Name.Namespace, Text, Punctuation), 'funclist'),
            # import X
            (r'[\w.]+', Name.Namespace, '#pop'),
        ],
        'module': [
            (r'\s+', Text),
            (r'([' + uni.Lu + r'][\w.]*)(\s+)(\()',
             bygroups(Name.Namespace, Text, Punctuation), 'funclist'),
            (r'[' + uni.Lu + r'][\w.]*', Name.Namespace, '#pop'),
        ],
        'funclist': [
            (r'\s+', Text),
            (r'[' + uni.Lu + r']\w*', Keyword.Type),
            (r'(_[\w\']+|[' + uni.Ll + r'][\w\']*)', Name.Function),
            (r'--(?![!#$%&*+./<=>?@^|_~:\\]).*?$', Comment.Single),
            (r'\{-', Comment.Multiline, 'comment'),
            (r',', Punctuation),
            (r'[:!#$%&*+.\\/<=>?@^|~-]+', Operator),
            # (HACK, but it makes sense to push two instances, believe me)
            (r'\(', Punctuation, ('funclist', 'funclist')),
            (r'\)', Punctuation, '#pop:2'),
        ],
        # NOTE: the next four states are shared in the AgdaLexer; make sure
        # any change is compatible with Agda as well or copy over and change
        'comment': [
            # Multiline Comments
            (r'[^-{}]+', Comment.Multiline),
            (r'\{-', Comment.Multiline, '#push'),
            (r'-\}', Comment.Multiline, '#pop'),
            (r'[-{}]', Comment.Multiline),
        ],
        'character': [
            # Allows multi-chars, incorrectly.
            (r"[^\\']'", String.Char, '#pop'),
            (r"\\", String.Escape, 'escape'),
            ("'", String.Char, '#pop'),
        ],
        'string': [
            (r'[^\\"]+', String),
            (r"\\", String.Escape, 'escape'),
            ('"', String, '#pop'),
        ],
        'escape': [
            (r'[abfnrtv"\'&\\]', String.Escape, '#pop'),
            (r'\^[][' + uni.Lu + r'@^_]', String.Escape, '#pop'),
            ('|'.join(ascii), String.Escape, '#pop'),
            (r'o[0-7]+', String.Escape, '#pop'),
            (r'x[\da-fA-F]+', String.Escape, '#pop'),
            (r'\d+', String.Escape, '#pop'),
            (r'\s+\\', String.Escape, '#pop'),
        ],
    }
