#!/bin/sh

makeinfo configurator.texinfo

texi2pdf configurator.texinfo
texi2pdf configurator.texinfo

texi2html configurator.texinfo

texi2html configurator.texinfo --split=chapter --output=html
