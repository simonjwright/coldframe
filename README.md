# ColdFrame #

ColdFrame is an open-source code generator backend for use with UML tools.

In its present state, it's targeted at [ArgoUML](https://github.com/argouml-tigris-org/argouml)<sup>1</sup> and generates [Ada](http://www.adaic.org/) code frameworks.

The intermediate representation uses [XML](http://www.xml.com/).

The code generation uses the XSL Transformation ([XSLT](http://www.w3.org/Style/XSL/)) language, so - with an investment in time and thought - you can design your own transformation mechanisms to the programming language of your choice.

See [here](https://simonjwright.github.io/coldframe/) for more information.

There are [examples](examples/).

----

1: ArgoUML [version 0.35.1](https://github.com/argouml-tigris-org/argouml/releases/tag/VERSION_0_35_1) is recommended. Note, this _won't_ run on recent releases of macOS, because it relies on Java 6.
