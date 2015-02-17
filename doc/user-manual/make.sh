#!/bin/sh

makeinfo acme.texinfo

texi2pdf acme.texinfo
texi2pdf acme.texinfo

texi2html acme.texinfo

texi2html acme.texinfo --split=chapter --output=html
