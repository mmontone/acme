# ACME

**ACME** is an Application Configuration ManagEr. It is written in Python and provides a Tk GUI for configurations manipulation.

## Overview

The idea is to define configuration schemas and get a proper way of:

- Sharing and versioning your project’s configuration schemas, but not your configurations. That way, you avoid overwriting configurations from different developers. Each developer has his own configurations that need to match the configuration schemas in the project. Whenever a project’s configuration schema changes, each developer is reponsible for updating his configurations to match the new schemas.
- Being able to define configuration schemas from the GUI, with no need for programming in most cases.
- Provide configurations documentation and validation.
- Edit configurations from a GUI.
- Define your own option configurations types and provide validation for them.


## Install

Using pip:
```
sudo pip install https://github.com/mmontone/acme/archive/master.zip
```

or download Acme and then run ``sudo python setup.py install``. That installs ``acme`` binary on ``/usr/local/bin``. After that, you can run acme typing ``acme`` at the shell.

## Documentation

User manual: [HTML](http://mmontone.github.io/acme/doc/user-manual/acme.html) [PDF](http://mmontone.github.io/acme/doc/user-manual/acme.pdf)

Developer manual: [HTML](http://mmontone.github.io/acme/doc/developer-manual/html/index.html) [PDF](http://mmontone.github.io/acme/doc/developer-manual/acme.pdf)

## Quick introduction ##

### Running

Acme is run invoking acme command from the command line. By default, it runs in normal mode; that means, it opens a GUI for adding, removing and editing configurations.

A configuration schemas files is required. The schemas file contains a serialization of the configuration schemas in XML format.

By default, Acme looks for acme.schema in the current directory. It shows an error if it can not find it. A different file or location can be specified through the –schemas SCHEMAS option. If the schemas file is found, then it is parsed and loaded. 

Apart from that, Acme maintains configurations in another file, which by default is acme.config. That file contains the configurations serialized in XML format. It can be specified to be something else WITH the -–configs CONFIGS option. 

Acme can be run in three different modes fundamentally.

- **Normal mode**: this mode is invoked running acme with no special arguments from the command line (apart from the schema and configs arguments). In this mode, the standard configuration navigation UI is opened. This UI is meant for end users. The user can create, remove and edit his configurations from here. He doesn’t need to know about how to build a configuration schema (although that is not difficult at all, as we will see.) Apart from that, when editing the configuration, the user gets a (hopefully) decent UI with custom option editors depending on the type of options and validation.

    This is an example of Acme running in normal mode: 

![configs](https://raw.github.com/mmontone/acme/master/doc/user-manual/images/acme1.png)

- **Setup mode**: this mode is invoked running acme with the --setup option from the command line. In this mode, the configuration schemas navigator UI is opened. The developer can create, remove and edit configuration schemas from here. Configuration schemas are descriptions of how configurations should be, with nested sections and different type of options. He can build the application specific configuration schemas from here.

    This is an example of Acme running in setup mode: 

![schemas](https://raw.github.com/mmontone/acme/master/doc/user-manual/images/schemas1.png)

- **Full mode:** this mode is invoked running acme with the –full option from the command line. In this mode, both the configurations navigator and the configurations schemas navigator are available in two different tabs.

   This is an example of Acme running in full mode: 

![full](https://raw.github.com/mmontone/acme/master/doc/user-manual/images/full.png)

### Configuration schemas ###

Configuration schemas define the configurations structure. They have a name, a list of parents, and a list of sections with options definitions.

![newschema](https://raw.github.com/mmontone/acme/master/doc/user-manual/images/newschema.png)

Configuration schemas can be composed by inheriting from multiple parents. Configuration sections from the parents appear in the child configuration schema. For instance, a full stack web framework configuration schema could inherit from a generic Web schema for web server configuration, and another Database schema for database connection configuration.

Configuration schemas have sections, each containing other sections and schema options. The schemas sections and options can be manipulated in the tree appearing on the left of the configuration schemas navigator.

![schemas_navigation](https://raw.github.com/mmontone/acme/master/doc/user-manual/images/schemas_navigation.png)

Each configuration schema section has a name, a documentation, subsections and a list of option schemas. Sections, subsections, and sections' options can all be added and removed from the tree widget on the left of the schemas navigator.

Options in schemas have a name and a type. 

![schema_option](https://raw.github.com/mmontone/acme/master/doc/user-manual/images/schema_option.png)

The type of option determines the possible values that can be assigned to it in configurations. The option type can be String, Number, Boolean, Email, URI, Filename, Directory, Color, Timezone, Language, Country, Currency, Date, Time, Datetime, etc. Some of them will be described later. When editing configurations, each option is edited with its corresponding editor, depending on the type of option. For instance, options of type Date are edited using a calendar.

![option_editing](https://raw.github.com/mmontone/acme/master/doc/user-manual/images/option_editing.png)

Apart from name and type, schema options specify if it required for the option to be assigned a value in the configuration. Also, they can have a default value in case the user doesn't assign one in the configuration. 

They also have a documentation string. This is very important for the end user to know the option meaning.

### Configurations ###

Configurations are instances of Configuration schemas, much like objects are instances of classes in object oriented programming languages. That is, configurations structure is determined by the configuration schema they belong to. In particular, their sections are that of their configuration schemas; and their options values depend on the option schemas defined in the configruration schemas.

Configurations belong to one and only one configuration schema, and that's compulsory, as it can be expected. Besides, configurations can inherit from each other: a configuration can inherit from another configuration and overwrite its parent options values. A configuration can have one and only one parent, o no parent; this is different from configuration schemas, that can have several parents. An understandable restriction is that the configuration parent schema has to be the same that the configuration schema.

Configurations can be added and removed from the list appering on the left of the configurations navigator.

![configs](https://raw.github.com/mmontone/acme/master/doc/user-manual/images/acme1.png)

Configurations can be loaded and saved. They are serialized in XML format. The default filename is ``acme.config``, but it can be changed if desired.

Configuration options editing happens on the right panel of the configurations navigator. A specific option editor is offered for each type of option, and each option documentation is displayed too. When trying to save a configuration section, it is ensured that required options (options declared with ``required`` enabled in the configuration schema) are filled. Options that are not currently set have their default value, if any. Also, options can be ``set`` and ``unset``. Setting a configuration option means setting the configuration option in the current configuration to the value being shown in the option editor. Unsetting a configuration option means removing the option value setting from the current configuration.

### Examples ###

There are some example configurations and schemas to see what the application is about.

Run ``acme``, ``acme --full``, and ``acme --setup`` from the ``doc/example`` directory.
