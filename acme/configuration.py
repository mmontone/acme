"""

.. module:: configuration
     :synopsis: Implements the configurations model.

.. moduleauthor:: Mariano Montone <marianomontone@gmail.com>

"""

#import xml.etree.ElementTree as et
from util import *
from lxml import etree as et
import datetime
import dependencies
import logging
import email.utils
import urlparse

class ConfigurationSchema(object):
    """Configuration schemas define the configurations structure. They have a name, a list of parents, and a list of sections with options definitions.

Configuration schemas can be composed by inheriting from multiple parents. Configuration sections from the parents appear in the child configuration schema. For instance, a full stack web framework configuration schema could inherit from a generic Web schema for web server configuration, and another Database schema for database connection configuration.

Configuration schemas have sections, each containing other sections and schema options. The schemas sections and options can be manipulated in the tree appearing on the left of the configuration schemas navigator.
    """

    _configuration_schemas = {}
    
    @classmethod
    def get_named(cls, name):
        """Get the configuration schema registered with name *name*"""
        schema = cls._configuration_schemas.get(name)
        if schema is None:
            raise Exception(name + ' schema not found')
        else:
            return schema
    
    @classmethod
    def register_schema(cls, schema):
        """Register a schema, globally. This is called automatically when a configuration schema is created."""
        logging.info("Registering schema " + schema.name)
        cls._configuration_schemas[schema.name] = schema
        
    @classmethod
    def unregister_schema(cls, schema):
        """Unregister a schema, globally."""
        del cls._configuration_schemas[schema.name]
        
    @classmethod
    def configuration_schemas(cls):
        """Return the list of globally registered configuration schemas"""
        return cls._configuration_schemas.values()        
    
    def __init__(self, name='', **args):
        self._name = name
        self._direct_sections = []
        self._documentation = args.get('documentation') or ""
        self._parents = args.get("parents") or []
        ConfigurationSchema.register_schema(self)
                
    def section(self, section):
        """Add section *section* to schema"""
        self._direct_sections.append(section)
        section.parent = self
        return self
    
    def direct_sections(self):
        """Return schema's direct sections (without looking at schema's parents sections)"""
        return self._direct_sections
    
    def sections(self):
        """Return schema's sections recursively considering its parents"""
        sections = []
        sections.extend(self.direct_sections())
        
        for parent in self.parents():
            sections.extend(parent.sections())
        return sections
    
    def move_section_backwards(self, section):
        """Move *section* backwards. For moving sections from UI"""
        index = self._direct_sections.index(section)
        
        if index > 0:
            prev_section = self._direct_sections[index - 1]
            self._direct_sections[index - 1] = section
            self._direct_sections[index] = prev_section 
            
    def move_section_forward(self, section):
        """Move *section* forward (reorder). For moving sections from UI"""
        index = self._direct_sections.index(section)
        
        if index < len(self._direct_sections) - 1:
            next_section = self._direct_sections[index - 1]
            self._direct_sections[index + 1] = section
            self._direct_sections[index] = next_section 
    
    def get_section(self, name):
        """Get section with name"""
        return next((s for s in self._direct_sections if s.name == name), None)
        
    def remove_section(self, section):
        """Remove *section* from the schema"""
        self._direct_sections.remove(section)
            
    def parents(self):
        """Returns the schema's parents list"""
        return map(lambda name: ConfigurationSchema.get_named(name), self._parents)
    
    def set_parents(self, parents):
        """Sets the schema's parents to *parents* list"""
        self._parents = []
        for parent in parents:
            self.add_parent(parent)
        return self
    
    def add_parent(self, parent):
        """Adds *parent* to the list of schema's parents"""
        if isinstance(parent, str):
            self._parents.append(parent)
        else:
            self._parents.append(parent.name)
        
    @property
    def name(self):
        """The schema's name"""
        return self._name
    
    @name.setter
    def name(self, value):
        self._name = value
        return self
    
    @property
    def documentation(self):
        """The schema's documentation"""
        return self._documentation
    
    @documentation.setter
    def documentation(self, value):
        self._documentation = value
        return self
    
    def remove(self):
        ConfigurationSchema.unregister_schema(self)
    
    def __str__(self):
        return self.name 
    
    def path(self):
        return ()
    
    def option_in_path(self, path):
        section = next((s for s in self.sections() if s.name == path[0]), None)
        if not section:
            raise Exception('Section ' + path[1] + ' not found')
        return section.option_in_path(path[1:])
        
class ConfigurationSchemaSection(object):
    """Configuration schema section"""
    
    def __init__(self, name='', **args):
        self._name = name
        self._subsections = []
        self._options = []
        self._documentation = args.get('documentation') or ''
        self._parent = args.get('parent') or None
        self._dependency_expression = None
        
    @property
    def name(self):
        """Section name"""
        return self._name
    
    @name.setter
    def name(self, value):
        self._name = value
        return self
        
    def subsections(self):
        """Return the section's subsections"""
        return self._subsections
            
    def add_section(self, section):
        """Adds section *section* as a subsection"""
        self._subsections.append(section)
        section.parent = self
        return self
    
    def remove_section(self, section):
        """Removes the subsection *section*"""
        self._subsections.remove(section)
    
    def options(self):
        """Returns the section's options list"""
        return self._options
    
    def add_option(self, option):
        """Adds option *option* to the section"""
        self._options.append(option)
        option.section = self
        return self
    
    def remove_option(self, option):
        """Removes option *option* from the section"""
        self._options.remove(option)        
        
    def get_option(self, name):
        """Gets section option with name *name* from the section.
        If it is not found, None is return"""
        return next((opt for opt in self._options if opt.name == name), None)
        
    @property
    def documentation(self):
        """The section's documentation"""
        return self._documentation
    
    @documentation.setter
    def documentation(self, value):
        self._documentation = value
        return self
    
    @property
    def parent(self):
        """The section's parent. It can be the configuration schema if it is a top-level section, of the parent section if it is a subsection"""
        return self._parent
    
    @parent.setter
    def parent(self, value):
        self._parent = value
        return self
    
    @property
    def dependency_expression(self):
        """The section's dependency expression"""
        return self._dependency_expression
    
    @dependency_expression.setter
    def dependency_expression(self, value):
        self._dependency_expression = value
    
    def remove(self):
        self.parent.remove_section(self)
        
    def path(self):
        return self.parent.path() + (self.name,)
    
    def schema(self):
        if isinstance(self.parent, ConfigurationSchema):
            return self.parent
        else:
            return self.parent.schema()
        
    def move_backwards(self):
        self.schema().move_section_backwards(self)
        
    def move_forward(self):
        self.schema().move_section_forward(self)
        
    def move_option_backwards(self, option):
        index = self._options.index(option)
        
        if index > 0:
            prev_option = self._options[index - 1]
            self._options[index - 1] = option
            self._options[index] = prev_option 
            
    def move_option_forward(self, option):
        index = self._options.index(option)
        
        if index < len(self._options) - 1:
            next_option = self._options[index - 1]
            self._options[index + 1] = option
            self._options[index] = next_option 
    
    def option_in_path(self, path):
        if len(path) > 1:
            # There are subsections in path
            subsection = next((s for s in self._subsections if s.name == path[0]), None)
            
            if not subsection:
                raise Exception('Subsection not found ' + path[0])
            
            return subsection.option_in_path(path[1:])
        else:
            # Find the option
            option = next((o for o in self._options if o.name == path[0]), None)
            
            if not option:
                raise Exception('Option not found ' + path[0])
            
            return option
        
    def validate(self, config):
        # Validate the section using config
        errors = {}
        for option in self.options():
            option_value, origin = config.option_value(option)
            if config.option_is_enabled(option) and option.is_required and option.default_value is None and option_value is None:
                errors[option.name] = {'option':option, 'message': option.path_string() + ' is required'}
            else:
                if option_value is not None:
                    error = option.validate(option_value)
                    if error is not None:
                        errors[option.name] = {'option':option, 'message' : option.path_string() + ": " + error}             
                
        if len(errors.values()) > 0:
            return errors
        else:
            return None               

class ConfigurationSchemaOption(object):
    def __init__(self, name, option_type, **args):
        self._name = name
        self._option_type = option_type
        self._documentation = args.get('documentation') or ''
        self._section = None
        self._is_required = args.get('required') or True
        self._default_value = args.get('default_value') or None
        self._dependency_expression = None
    
    @property
    def name(self):
        return self._name
    
    @name.setter
    def name(self, value):
        self._name = value
        return self
        
    @property
    def option_type(self):
        return self._option_type
    
    @option_type.setter
    def option_type(self, value):
        self._option_type = value
        return self
    
    @property
    def documentation(self):
        return self._documentation
    
    @documentation.setter
    def documentation(self, value):
        self._documentation = value
        return self
    
    @property
    def section(self):
        return self._section
    
    @section.setter
    def section(self, value):
        self._section = value
        return self
    
    def remove(self):
        self.section.remove_option(self)
        
    @property
    def is_required(self):
        return self.default_value is None and self._is_required
    
    @is_required.setter
    def is_required(self, value):
        self._is_required = value
        return self
    
    @property
    def default_value(self):
        return self._default_value
    
    @default_value.setter
    def default_value(self, value):
        self._default_value = value
        return self
    
    @property
    def dependency_expression(self):
        return self._dependency_expression
    
    @dependency_expression.setter
    def dependency_expression(self, value):
        self._dependency_expression = value
    
    def path(self):
        return self.section.path() + (self.name,)
    
    def path_string(self):
        return '.'.join(self.path())
    
    def schema(self):
        return self.section.schema()
    
    def move_forward(self):
        self.section.move_option_forward(self)
        
    def move_backwards(self):
        self.section.move_option_backwards(self)
        
    def parse_value(self, value):
        return self._option_type.parse_value(value)
        
    def unparse_value(self, value):
        return self.option_type.unparse_value(value)
    
    def display_value(self, value):
        return self.option_type.display_value(value)
    
    def validate(self, value):
        return self.option_type.validate(value)
    
class OptionType(object):
    _name = None
    
    @classmethod
    def option_name(cls):
        return cls._name
    
    @classmethod
    def get_named(cls, name):
        option_type = next((option_type for option_type in all_subclasses(cls) if option_type.option_name() == name), None)
        if not option_type:
            raise Exception('No option type named ' + name)
        return option_type
    
    @classmethod
    def option_types(cls):
        return all_subclasses(OptionType)
    
    @property
    def name(self):
        return self.__class__.option_name()
    
    def parse_value(self, value):
        return value
    
    def unparse_value(self, value):
        return str(value)
    
    def display_value(self, value):
        return str(value)
    
    def validate(self, value):
        return None
    
    def __str__(self):
        return self.name
   
class StringOptionType(OptionType):
    _name = "String"
    
    def accept(self, visitor):
        return visitor.visit_StringOptionType(self)
    
class NumberOptionType(OptionType):
    _name = "Number"
    
    def accept(self, visitor):
        return visitor.visit_NumberOptionType(self)
        
    def parse_value(self, value):
        return int(value)
    
    def validate(self, value):
        if not isinstance(value, int):
            return "Should be a number"        
    
class BooleanOptionType(OptionType):
    _name = "Boolean"
    
    def accept(self, visitor):
        return visitor.visit_BooleanOptionType(self)
        
    def parse_value(self, value):
        return value == 'True'
    
    def validate(self, value):
        if not isinstance(value, bool):
            return "Should be a boolean"
   
class EmailOptionType(OptionType):
    _name = "Email"
    
    def accept(self, visitor):
        return visitor.visit_EmailOptionType(self)
    
    def validate(self, value):
        x, y = email.utils.parseaddr(value)
        if x == '' and y == '':
            return str(value) + " is an invalid email"
    
class URIOptionType(OptionType):
    _name = "URI"
    
    def accept(self, visitor):
        return visitor.visit_URIOptionType(self)
    
    def validate(self, value):
        uri = urlparse.urlparse(value)
        #print "URI validation result: " + str(uri)
    
class FilenameOptionType(OptionType):
    _name = "Filename"
    
    def accept(self, visitor):
        return visitor.visit_FilenameOptionType(self)
    
class DirectoryOptionType(OptionType):
    _name = "Directory"
    
    def accept(self, visitor):
        return visitor.visit_DirectoryOptionType(self)
    
class ColorOptionType(OptionType):
    _name = "Color"
    
    def accept(self, visitor):
        return visitor.visit_ColorOptionType(self)
    
class TimezoneOptionType(OptionType):
    _name = "Timezone"
    
    def accept(self, visitor):
        return visitor.visit_TimezoneOptionType(self)
    
class CountryOptionType(OptionType):
    _name = "Country"
    
    def accept(self, visitor):
        return visitor.visit_CountryOptionType(self)
    
class LanguageOptionType(OptionType):
    _name = "Language"
    
    def accept(self, visitor):
        return visitor.visit_LanguageOptionType(self)
    
class CurrencyOptionType(OptionType):
    _name = "Currency"
    
    def accept(self, visitor):
        return visitor.visit_CurrencyOptionType(self)
    
class DateOptionType(OptionType):
    _name = "Date"
    
    def accept(self, visitor):
        return visitor.visit_DateOptionType(self)
    
    def parse_value(self, value):
        return datetime.datetime.strptime(value, "%d/%m/%Y")  
    
    def unparse_value(self, value):
        return value.strftime("%d/%m/%Y")
    
    def display_value(self, value):
        return value.strftime("%d/%m/%Y")         
    
class TimeOptionType(OptionType):
    _name = "Time"
    
    def accept(self, visitor):
        return visitor.visit_TimeOptionType(self)
    
    def parse_value(self, value):
        return datetime.datetime.strptime(value, "%H:%M:%S")
    
    def unparse_value(self, value):
        return value.strftime("%H:%M:%S")
    
    def display_value(self, value):
        return value.strftime("%H:%M:%S")
    
class DatetimeOptionType(OptionType):
    _name = "Datetime"
    
    def accept(self, visitor):
        return visitor.visit_DatetimeOptionType(self)
    
    def parse_value(self, value):
        tuple = eval(value)
        return (datetime.datetime.strptime(tuple[0], "%d/%m/%Y"),
                datetime.datetime.strptime(tuple[1], "%H:%M:%S"))
     
    def unparse_value(self, value):
        datestring = value[0].strftime("%d/%m/%Y")
        timestring = value[1].strftime("%H:%M:%S")
        return "('" + datestring + "','" + timestring + "')"
    
    def display_value(self, value):
        datestring = value[0].strftime("%d/%m/%Y")
        timestring = value[1].strftime("%H:%M:%S")
        return datestring + " " + timestring                       
    
class ChoiceOptionType(OptionType):
    _name = "Choice"
    
    def __init__(self, options=None):
        
        if options is None:
            self._options=[]
        else:
            self._options = options
        
    def options(self):
        return self._options
    
    def add_option(self, option):
        logging.debug("Adding option " + option + " to " + str(self))
        self._options.append(option)
    
    def accept(self, visitor):
        return visitor.visit_ChoiceOptionType(self)
    
    def validate(self, value):
        if not value in self.options():
            return '\'' + str(value) + '\' is not a valid choice (' + ', '.join(self.options()) + ')'
    
class ListOptionType(OptionType):
    _name = "List"
    
    def __init__(self, options=None):
        if options is None:
            self._options = []
        else:
            self._options = options
        
    def options(self):
        return self._options
    
    def add_option(self, option):
        self._options.append(option)
    
    def accept(self, visitor):
        return visitor.visit_ListOptionType(self)
    
    def parse_value(self, value):
        return eval(value)
    
    def display_value(self, value):
        return ', '.join(value)
    
    def validate(self, value):
        vals = list(set(value) - set(self.options()))
        if len(vals) > 0:
            return str(vals) + ' are not a valid list members (' + ', '.join(self.options()) + ')'
        
class MaybeOptionType(OptionType):
    _name = "Maybe"
    
    def __init__(self, option_type=None):
        OptionType.__init__(self)
        
        self._option_type = option_type
        
    @property
    def option_type(self):
        return self._option_type 
    
    @option_type.setter
    def option_type(self, value):
        self._option_type = value
        return self
    
    def accept(self, visitor):
        return visitor.visit_MaybeOptionType(self)
        
class OneOfOptionType(OptionType):
    _name = "One of"
    
    def __init__(self):
        OptionType.__init__(self)
               
    @property
    def option_types(self):
        return self._option_types 
    
    @option_types.setter
    def option_types(self, value):
        self._option_types = value
        return self       
    
    def accept(self, visitor):
        return visitor.visit_OneOfOptionType(self)
    
class ManyOptionType(OptionType):
    _name = "Many"   
    
    def __init__(self, option_type=None):
        OptionType.__init__(self)
        
        self._option_type = option_type
        
    @property
    def option_type(self):
        return self._option_type 
    
    @option_type.setter
    def option_type(self, value):
        self._option_type = value
        return self
    
    def accept(self, visitor):
        return visitor.visit_ManyOptionType(self)
    
    def parse_value(self, str):
        return eval(str)
    
    def display_value(self, value):
        return ', '.join(value)
    
    def __str__(self):
        return self.name + '(' + str(self.option_type) + ')'
    
class CustomOptionType(OptionType):
    
    _custom_option_types = {}
    
    @classmethod
    def get_named(cls, name):
        option_type = cls._custom_option_types.get(name)
        if option_type is None:
            raise Exception(name + ' custom option type not found')
        else:
            return option_type
    
    @classmethod
    def register_custom_option_type(cls, option_type):
        logging.info("Registering custom option type " + option_type.name)
        cls._custom_option_types[option_type.name] = option_type
        
    @classmethod
    def unregister_custom_option_type(cls, option_type):
        del cls._custom_option_types[option_type.name]
        
    @classmethod
    def custom_option_types(cls):
        return cls._custom_option_types.values()  
          
    def __init__(self, name, attributes=None):
        OptionType.__init__(self)
        
        self._name = name
        
        if attributes is not None:
            self._attributes = attributes
        else:
            self._attributes = []
            
        CustomOptionType.register_custom_option_type(self)
            
    @property
    def attributes(self):
        return self._attributes 
    
    @attributes.setter
    def attributes(self, value):
        self._attributes = value
        return self
    
    def add_attribute(self, name, type):
        self._attributes.append((name, type))
    
    @property
    def name(self):
        return self._name 
    
    @name.setter
    def name(self, value):
        self._name = value
        return self
    
    def accept(self, visitor):
        return visitor.visit_CustomOptionType(self)
    
      
class Configuration(object):
    
    _configurations = {}
    
    @classmethod
    def get_named(cls, name):
        config = cls._configurations.get(name)
        if config is None:
            raise Exception(name + ' configuration not found')
        else:
            return config
    
    @classmethod
    def register_config(cls, config):
        logging.info("Registering config " + config.name)
        cls._configurations[config.name] = config
        
    @classmethod
    def unregister_config(cls, config):
        del cls._configurations[config.name]
        
    @classmethod
    def configurations(cls):
        return cls._configurations.values()    
    
    def __init__(self, name='', schema=None, **options):
        self._name = name
        if schema is not None:
            self._schema = schema.name
        else:
            self._schema = None
        self._parent = options.get('parent') or None
        self._options = {}
        self._documentation = options.get('documentation') or ''
        Configuration.register_config(self)
        
    @property
    def name(self):
        return self._name
    
    @name.setter
    def name(self, value):
        self._name = value
        return self
    
    @property
    def schema(self):
        if self._schema is not None: 
            return ConfigurationSchema.get_named(self._schema)
    
    @schema.setter
    def schema(self, value):
        self._schema = value.name
        return self
    
    @property
    def parent(self):
        if self._parent is None:
            return None
        else:
            return Configuration.get_named(self._parent)
    
    @parent.setter
    def parent(self, value):
        if value is None:
            self._parent = None
        elif isinstance(value, str):
            self._parent = value
        else:
            self._parent = value.name
            
        return self
    
    @property
    def documentation(self):
        return self._documentation
    
    @documentation.setter
    def documentation(self, value):
        self._documentation = value
        return self
    
    def set_option_value(self, schema_option, value):
        logging.info(self.name + ' configuration: Setting ' + schema_option.path_string() + ' to ' + str(value))
        option = ConfigurationOption(schema_option, value=value)
        self._options[schema_option] = option
        
    def unset_option(self, schema_option):
        if self._options.get(schema_option):
            del self._options[schema_option]
        
    def option_value(self, schema_option):
        option = self._options.get(schema_option, None)
        if option:
            # Return a tuple, the option value and its origin (the configuration that has its value)
            return option.value, self
        else:
            if self.parent:
                return self.parent.option_value(schema_option)
            else:
                return None, None
            
    def option_is_enabled(self, schema_option):
        if schema_option.dependency_expression is None:
            return True
        else:
            logging.debug("Option enabled " + schema_option.path_string() + ": " + str(schema_option.dependency_expression.evaluate(self))) 
            return schema_option.dependency_expression.evaluate(self)
        
    def sections(self):
        return self.schema.sections()
    
    def options(self):
        return self._options.values()
    
    def validate(self):
        errors = []
        for section in self.sections():
            section_errors = section.validate(self)
            if section_errors:
                errors.extend(section_errors.values())
        if len(errors) > 0:
            logging.info('Configuration ' + self.name + ' is not valid: ' + str(errors))
            return errors
        else:
            logging.info('Configuration ' + self.name + ' is valid')
            return None
        
    def __str__(self):
        return self.name        
        
class ConfigurationOption(object):
    
    def __init__(self, schema, **options):
        self._schema = schema
        self._value = None
        if options.get('value') is not None:
            self._value = options.get('value')
        
    @property
    def schema(self):
        return self._schema
    
    @schema.setter
    def schema(self, value):
        self._schema = value
        return self
    
    @property
    def name(self):
        return self._schema.name
    
    @property
    def value(self):
        return self._value
    
    @value.setter
    def value(self, value):
        self._value = value
        return self
    
    def path(self):
        return self.schema.path()
    
    def path_string(self):
        return self.schema.path_string()
        
    def __hash__(self):
        return hash(self.path())

    def __eq__(self, other):
        return self.path() == other.path()
    
    def __str__(self):
        return self.name
    
    def unparse_value(self, value):
        return self.schema.unparse_value(value)
    
    def display_value(self, value):
        return self.schema.display_value(value)
    
    def validate(self):
        return self.schema.validate(self.value)
    
class Serializer:
    pass

class XMLSerializer(Serializer):
    pass

class ConfigurationsXMLSerializer(XMLSerializer):
    def __init__(self):
        self._root = et.Element('configurations')
    
    def serialize(self, config):
        config_element = et.SubElement(self._root, 'configuration')
        config_element.attrib['name'] = config.name
                
        schema = et.SubElement(config_element, 'schema')
        schema.attrib['name'] = config.schema.name
        
        if config.parent:
            parent = et.SubElement(config_element, 'parent')
            parent.attrib['name'] = config.parent.name
        
        for option in config.options():
            option_elem = et.SubElement(config_element, 'option')
            option_elem.attrib['path'] = option.path_string()
            option_elem.attrib['value'] = self.serialize_option_value(option)
            
    def serialize_option_value(self, option):
        return option.unparse_value(option.value)
       
    def write(self, recipient):
        tree = et.ElementTree(self._root)
        tree.write(recipient, pretty_print=True)
        
class ConfigurationsXMLUnserializer():
    def __init__(self):
        self._configs = []
                   
    def read(self, source):       
        self._tree = et.parse(source)
        self.unserialize()
        return self._configs
                
    def unserialize(self):
        for config in self._tree.getroot():
            name = config.attrib['name']
            doc = config.findtext('documentation')
                       
            schema_name = config.find('schema').attrib['name']
            schema = ConfigurationSchema.get_named(schema_name)
            
            cfg = Configuration(name, schema, documentation=doc)
            
            parent = config.find('parent')
            if parent is not None:
                cfg.parent = parent.attrib['name']
                       
            for option in config.iterchildren(tag='option'):
                path = option.attrib['path'].split('.')
                opt = schema.option_in_path(path)
                value = opt.option_type.parse_value(option.attrib['value'])
                logging.debug('Unserialize: Setting ' + str(opt) + ' value: ' + str(value))
                cfg.set_option_value(opt, value)
                
            self._configs.append(cfg)
                  
        return self._configs          

class ConfigurationSchemasXMLSerializer(XMLSerializer):
    def __init__(self):
        self._root =  et.Element("schemas")
          
    def serialize(self, schema):
        schema_element = et.SubElement(self._root, "schema")
        schema_element.attrib['name'] = schema.name
        doc = et.SubElement(schema_element, 'documentation')
        doc.text = schema.documentation
        for parent in schema.parents():
            parent_element = et.SubElement(schema_element, 'parent')
            parent_element.attrib['name'] = parent.name
            
        for section in schema.direct_sections():
            self.serialize_section(section, schema_element)
                        
    def serialize_section(self, section, sections):
        section_element = et.SubElement(sections, 'section')
        section_element.attrib['name'] = section.name
        doc = et.SubElement(section_element, 'documentation')
        doc.text = section.documentation
               
        for option in section.options():
            self.serialize_option(option, section_element)
        
        for subsection in section.subsections():
            self.serialize_section(subsection, section_element)
    
    def serialize_option(self, option, options):
        option_elem = et.SubElement(options, 'option')
        option_elem.attrib['name'] = option.name
        doc = et.SubElement(option_elem, 'documentation')
        doc.text = option.documentation
        option_type = et.SubElement(option_elem, 'type')
        self.serialize_option_type(option.option_type, option_type)
        required = et.SubElement(option_elem, 'required')
        required.text = 'True' if option.is_required else 'False'
        if option.default_value is not None:
            default_value = et.SubElement(option_elem, 'default')
            default_value.text = option.option_type.unparse_value(option.default_value)
        if option.dependency_expression is not None:
            dependency_expression = et.SubElement(option_elem, 'dependency')
            dependency_expression.text = str(option.dependency_expression)
    
    def serialize_option_type(self, option_type, element):
        element.attrib['name'] = option_type.name
        self._option_elem = element
        
        visitor_method = getattr(self, "serialize" + option_type.name + "OptionType", None)
        if callable(visitor_method):
            visitor_method(option_type)     
    
    def serializeChoiceOptionType(self, option_type):
        for option in option_type.options():
            opt = et.SubElement(self._option_elem, 'option')
            opt.attrib['value'] = option
            
    def serializeListOptionType(self, option_type):
        for option in option_type.options():
            opt = et.SubElement(self._option_elem, 'option')
            opt.attrib['value'] = option
            
    def serializeManyOptionType(self, option_type):
        many_type = option_type.option_type
        many_type_elem = et.SubElement(self._option_elem, 'type')
        many_type_elem.attrib['name'] = many_type.name
        self.serialize_option_type(option_type.option_type, many_type_elem) 
            
    def write(self, recipient):
        tree = et.ElementTree(self._root)
        tree.write(recipient, pretty_print=True)
        
class ConfigurationSchemasXMLUnserializer():
    
    def __init__(self, **cfg):
        self._schemas = []
        self._tree = None
                
    def read(self, source):       
        self._tree = et.parse(source)
        self.unserialize()
        return self._schemas
                
    def unserialize(self):
        for schema in self._tree.getroot():
            name = schema.attrib['name']
            doc = schema.findtext('documentation')
            
            sch = ConfigurationSchema(name, documentation=doc)
            
            for parent in schema.iterchildren(tag='parent'):
                sch.add_parent(parent.attrib['name'])
                
            for section in schema.iterchildren(tag='section'):
                sch.section(self.unserialize_section(section))
                
            self._schemas.append(sch)
            
        return self._schemas            
            
    def unserialize_section(self, section_elem):
        name = section_elem.attrib['name']
        doc = section_elem.findtext('documentation')
        logging.debug("Unserializing section " + name)
        section = ConfigurationSchemaSection(name, documentation=doc)
        
        for option in section_elem.iterchildren(tag='option'):
            section.add_option(self.unserialize_option(option))
            
        for subsection in section_elem.iterchildren(tag='section'):
            section.add_section(self.unserialize_section(subsection))
            
        return section
    
    def unserialize_option(self, option_elem):
        name = option_elem.attrib['name']
        doc = option_elem.findtext('documentation')
        is_required = option_elem.findtext('required')
        default_value = option_elem.findtext('default')
        option_type_elem = option_elem.find('type')
        option_type = self.unserialize_option_type(option_type_elem)
        dependency_expression = option_elem.findtext('dependency')
                
        option = ConfigurationSchemaOption(name, option_type, documentation=doc)
        
        option.is_required = is_required is not None and eval(is_required)
        
        if default_value is not None:
            option.default_value = option_type.parse_value(default_value)
            
        if dependency_expression is not None:
            option.dependency_expression = DependencyExpressionParser.parse_expression(dependency_expression)
            
        return option
        
    def unserialize_option_type(self, option_type_elem):
        self._option_type_elem = option_type_elem
        
        name = option_type_elem.attrib['name']
        option_type_class = OptionType.get_named(name)
        
        option_type = option_type_class()
        
        visitor_method = getattr(self, "unserialize" + option_type.name + "OptionType", None)
        if callable(visitor_method):
            return visitor_method(option_type)
        else:
            return option_type        
       
    def unserializeChoiceOptionType(self, option_type):
        for option in self._option_type_elem.iterchildren(tag='option'):
            option_type.add_option(option.attrib['value'])
        return option_type
    
    def unserializeListOptionType(self, option_type):
        for option in self._option_type_elem.iterchildren(tag='option'):
            option_type.add_option(option.attrib['value'])
        return option_type
    
    def unserializeManyOptionType(self, option_type):
        many_type_elem = self._option_type_elem.find('type')
        
        many_type = self.unserialize_option_type(many_type_elem)
        option_type.option_type = many_type
        return option_type   
    
class YAMLSerializer(Serializer):
    pass

# Dependency expressions
class DependencyExpression(object):
    def __init__(self, **args):
        pass
    
    def evaluate(self, config):
        pass

class BooleanConnector(DependencyExpression):
    def __init__(self, arg1, arg2):
        DependencyExpression.__init__(self)
        self._arg1 = arg1
        self._arg2 = arg2
        
    @property
    def arg1(self):
        return self._arg1
    
    @property
    def arg2(self):
        return self._arg2
        

class DEAnd(BooleanConnector):
    def __str__(self):
        return str(self.arg1) + ' AND ' + str(self.arg2)
    
    def evaluate(self, config):
        return self.arg1.evaluate(config) and self.arg2.evaluate(config)
              
class DEOr(BooleanConnector):
    def __str__(self):
        return str(self.arg1) + ' OR ' + str(self.arg2)
    
    def evaluate(self, config):
        return self.arg1.evaluate(config) or self.arg2.evaluate(config)

class DEXor(BooleanConnector):
    def __str__(self):
        return str(self.arg1) + ' XOR ' + str(self.arg2)
    
    def evaluate(self, config):
        return self.arg1.evaluate(config) != self.arg2.evaluate(config)    

class BooleanOperation(DependencyExpression):
    def __init__(self, arg1, arg2):
        DependencyExpression.__init__(self)
        self._arg1 = arg1
        self._arg2 = arg2
        
    @property
    def arg1(self):
        return self._arg1
    
    @property
    def arg2(self):
        return self._arg2
        

class DEEqual(BooleanOperation):
    def __str__(self):
        return str(self.arg1) + ' = ' + str(self.arg2)
    
    def evaluate(self, config):
        return self.arg1.evaluate(config) == self.arg2.evaluate(config)    


class DEGreaterThan(BooleanOperation):
    def __str__(self):
        return str(self.arg1) + ' > ' + str(self.arg2)
    
    def evaluate(self, config):
        return self.arg1.evaluate(config) > self.arg2.evaluate(config)    

class DELowerThan(BooleanOperation):
    def __str__(self):
        return str(self.arg1) + ' < ' + str(self.arg2)
    
    def evaluate(self, config):
        return self.arg1.evaluate(config) < self.arg2.evaluate(config)
    
class DEDifferentFrom(BooleanOperation):
    def __str__(self):
        return str(self.arg1) + ' <> ' + str(self.arg2)
    
    def evaluate(self, config):
        return self.arg1.evaluate(config) <> self.arg2.evaluate(config)
    
class DEOptionPath(DependencyExpression):
    def __init__(self, path):
        DependencyExpression.__init__(self)
        self._path = path
        
    @property
    def path(self):
        return self._path
    
    def __str__(self):
        return '.'.join(self.path)  
    
    def evaluate(self, config):
        option = config.schema.option_in_path(self.path)
        value, origin = config.option_value(option)
        return value
    
class DEString(DependencyExpression):
    def __init__(self, value):
        DependencyExpression.__init__(self)
        self._value = value
        
    def __str__(self):
        return str(self._value)
    
    def evaluate(self, config):
        return self._value
    
class DEBoolean(DependencyExpression):
    def __init__(self, value):
        DependencyExpression.__init__(self)
        self._value = value
        
    def __str__(self):
        return str(self._value)
    
    def evaluate(self, config):
        return self._value
    
class DENumber(DependencyExpression):
    def __init__(self, value):
        DependencyExpression.__init__(self)
        self._value = value
        
    def __str__(self):
        return str(self._value)
    
    def evaluate(self, config):
        return self._value              
    
class DependencyExpressionParser(object):
    @classmethod
    def parse_expression(cls, expression):
        parser = dependencies.dependenciesParser(parseinfo=False)
        ast = parser.parse(expression, whitespace='', 
                           rule_name='boolexp', 
                           semantics=DependencyExpressionParser())
        return ast
    
    def bool_literal(self, ast):
        if ast == 'True':
            return DEBoolean(True)
        elif ast == 'False':
            return DEBoolean(False)
        else:
            raise Exception('Error parsing literal boolean ' + str(ast))
        
    def option_path(self, ast):
        if isinstance(ast, list):
            path = [ast[0]]
            path.extend(ast[1].path)
            return DEOptionPath(path)
        else:
            return DEOptionPath([ast])
        
    def identifier(self, ast):
        name = ast[0]
        if len(ast) > 1:
            for x in ast[1]:
                name = name + x
                
        return name.strip()
    
    def literal_string(self, ast):
        string = ''
        for x in ast:
            string = string + x
        return DEString(string)
    
    def boolexp(self, ast):
        #print "Boolexp: " + str(ast)
        if isinstance(ast, list):
            term = ast[0]
            connector = ast[1]
            exp = ast[2]
            
            if connector == 'AND':
                return DEAnd(term, exp)
            elif connector == 'OR':
                return DEOr(term, exp)
            elif connector == 'XOR':
                return DEXor(term, exp)
            else:
                raise Exception('Error parsing boolean expression ' + str(ast))
        else:
            return ast
        
    def boolterm(self, ast):
        #print "Boolterm: " + str(ast)
        if isinstance(ast, list):
            option_path = ast[0]
            operation = ast[1]
            value = ast[2]
            
            if operation == '=':
                return DEEqual(option_path, value)
            elif operation == '>':
                return DEGreaterThan(option_path, value)
            elif operation == '<':
                return DELowerThan(option_path, value)
            elif operation == '<>':
                return DEDifferentFrom(option_path, value)
        else:
            return ast
        
    def number(self, ast):
        str = ''
        for x in ast:
            str = string + x
        return DENumber(int(str))
