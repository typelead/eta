"""
Eta syntax highlighting for Pygments.
"""

from setuptools import setup

entry_points = """
[pygments.lexers]
eta = eta.highlight:EtaLexer
"""

setup(
    name         = 'pygments-eta',
    version      = '0.1',
    description  = __doc__,
    author       = "Rahul Muttineni",
    packages     = ['eta'],
    entry_points = entry_points
)
