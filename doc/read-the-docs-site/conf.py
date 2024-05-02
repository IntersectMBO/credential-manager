# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'Credential Manager'
copyright = '2024, Input Output Global Ltd.'
author = 'Jamie Bertram, Thomas Vellekoop, Tomasz Rybarczyk'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

master_doc = 'index'

extensions = [
    'sphinx_rtd_theme',
]

source_suffix = {
    '.rst': 'restructuredtext',
    '.md': 'markdown',
}

templates_path = ['_templates']
exclude_patterns = [
    'README.md'
]


# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'sphinx_rtd_theme'

html_theme_options = {
    'logo_only': False,
    'display_version': False,
    'prev_next_buttons_location': 'bottom',
    'style_external_links': False,
    # Toc options
    'collapse_navigation': False,
    'sticky_navigation': True,
    'navigation_depth': 4,
    'includehidden': True,
    'titles_only': False
}

html_context = {
  "display_github": True, # Add 'Edit on Github' link instead of 'View page source'
  "github_user": "IntersectMBO",
  "github_repo": "credential-manager",
  "github_version": "main",
  "conf_py_path": "/doc/read-the-docs-site/",
  "source_suffix": source_suffix,
}

html_static_path = ['_static']
