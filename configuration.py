#import xml.etree.ElementTree as et
from lxml import etree as et
import datetime
import dependencies

class ConfigurationSchema():
    _configuration_schemas = {}
    
    @classmethod
    def get_named(cls, name):
        schema = cls._configuration_schemas.get(name)
        if schema is None:
            raise Exception(name + ' schema not found')
        else:
            return schema
    
    @classmethod
    def register_schema(cls, schema):
        print "Registering schema " + schema.name
        cls._configuration_schemas[schema.name] = schema
        
    @classmethod
    def unregister_schema(cls, schema):
        del cls._configuration_schemas[schema.name]
        
    @classmethod
    def configuration_schemas(cls):
        return cls._configuration_schemas.values()        
    
    def __init__(self, name='', **args):
        self._name = name
        self._direct_sections = []
        self._documentation = args.get('documentation') or ""
        self._parents = args.get("parents") or []
        ConfigurationSchema.register_schema(self)
                
    def section(self, section):
        self._direct_sections.append(section)
        section.parent = self
        return self
    
    def direct_sections(self):
        return self._direct_sections
    
    def sections(self):
        sections = []
        sections.extend(self.direct_sections())
        
        for parent in self.parents():
            sections.extend(parent.sections())
        return sections        
    
    def get_section(self, name):
        return next((s for s in self._direct_sections if s.name == name), None)
        
    def remove_section(self, section):
        self._direct_sections.remove(section)
            
    def parents(self):
        return map(lambda name: ConfigurationSchema.get_named(name), self._parents)
    
    def set_parents(self, parents):
        self._parents = []
        for parent in parents:
            self.add_parent(parent)
        return self
    
    def add_parent(self, parent):
        if isinstance(parent, str):
            self._parents.append(parent)
        else:
            self._parents.append(parent.name)
        
    @property
    def name(self):
        return self._name
    
    @name.setter
    def name(self, value):
        self._name = value
        return self
    
    @property
    def documentation(self):
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
        return (self.name,)
    
    def option_in_path(self, path):
        section = next((s for s in self.sections() if s.name == path[1]), None)
        if not section:
            raise Exception('Section ' + path[1] + ' not found')
        return section.option_in_path(path[1:])
        
class ConfigurationSchemaSection:
    def __init__(self, name='', **args):
        self._name = name
        self._subsections = []
        self._options = []
        self._documentation = args.get('documentation') or ''
        self._parent = args.get('parent') or None
        self._dependency_expression = None
        
    @property
    def name(self):
        return self._name
    
    @name.setter
    def name(self, value):
        self._name = value
        return self
        
    def subsections(self):
        return self._subsections
            
    def add_section(self, section):
        self._subsections.append(section)
        section.parent = self
        return self
    
    def remove_section(self, section):
        self._subsections.remove(section)
    
    def options(self):
        return self._options
    
    def add_option(self, option):
        self._options.append(option)
        option.section = self
        return self
    
    def remove_option(self, option):
        self._options.remove(option)        
        
    def get_option(self, name):
        return next((opt for opt in self._options if opt.name == name), None)
        
    @property
    def documentation(self):
        return self._documentation
    
    @documentation.setter
    def documentation(self, value):
        self._documentation = value
        return self
    
    @property
    def parent(self):
        return self._parent
    
    @parent.setter
    def parent(self, value):
        self._parent = value
        return self
    
    @property
    def dependency_expression(self):
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
    
    def option_in_path(self, path):
        assert(path[0] == self.name)
        
        if len(path) > 2:
            # There are subsections in path
            subsection = next((s for s in self._subsections if s.name == path[1]), None)
            
            if not subsection:
                raise Exception('Subsection not found ' + path[1])
            
            return subsection.option_in_path(path[1:])
        else:
            # Find the option
            option = next((o for o in self._options if o.name == path[1]), None)
            
            if not option:
                raise Exception('Option not found ' + path[1])
            
            return option
        
    def validate(self, config):
        # Validate the section using config
        errors = {}
        for option in self.options():
            option_value, origin = config.option_value(option)
            if option.is_required and option.default_value is None and option_value is None:
                errors[option.name] = {'option':option, 'message': option.name + ' is required'}
        if len(errors.values()) > 0:
            return errors
        else:
            return None               

class ConfigurationSchemaOption:
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
        return self._is_required
    
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
    
class OptionType(object):
    _name = None
    
    @classmethod
    def option_name(cls):
        return cls._name
    
    @classmethod
    def get_named(cls, name):
        option_type = next((option_type for option_type in cls.__subclasses__() if option_type.option_name() == name), None)
        if not option_type:
            raise Exception('No option type named ' + name)
        return option_type
    
    @property
    def name(self):
        return self.__class__.option_name()
    
    def parse_value(self, value):
        return value
    
    def unparse_value(self, value):
        return str(value)
   
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
    
class BooleanOptionType(OptionType):
    _name = "Boolean"
    
    def accept(self, visitor):
        return visitor.visit_BooleanOptionType(self)
        
    def parse_value(self, value):
        return value == 'True'
   
class EmailOptionType(OptionType):
    _name = "Email"
    
class URIOptionType(OptionType):
    _name = "URI"
    
class FilenameOptionType(OptionType):
    _name = "Filename"
    
    def accept(self, visitor):
        return visitor.visit_FilenameOptionType(self)
    
class DirectoryOptionType(OptionType):
    _name = "Directory"
    
class ColorOptionType(OptionType):
    _name = "Color"
    
    def accept(self, visitor):
        return visitor.visit_ColorOptionType(self)
    
class TimezoneOptionType(OptionType):
    _name = "Timezone"
    
class CountryOptionType(OptionType):
    _name = "Country"
    
class LanguageOptionType(OptionType):
    _name = "Language"
    
class CurrencyOptionType(OptionType):
    _name = "Currency"
    
class DateOptionType(OptionType):
    _name = "Date"
    
class TimeOptionType(OptionType):
    _name = "Time"
    
class DatetimeOptionType(OptionType):
    _name = "Datetime"
    
    def accept(self, visitor):
        return visitor.visit_DatetimeOptionType(self)
    
    def unparse_value(self, string):
        return datetime.strptime(string, '%d/%m/%Y')
    
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
        print "Adding option " + option + " to " + str(self)
        self._options.append(option)
    
    def accept(self, visitor):
        return visitor.visit_ChoiceOptionType(self)
    
class ListOptionType(OptionType):
    _name = "List"
    
    def __init__(self, options=[]):
        self._options = options
        
    def options():
        return self._options
        
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
        print "Registering custom option type " + option_type.name
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
        print "Registering config " + config.name
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
            print "Option enabled " + schema_option.path_string() + ": " + str(schema_option.dependency_expression.evaluate(self)) 
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
        if len(errors) > 1:
            return errors
        else:
            return None
        
class ConfigurationOption():
    
    def __init__(self, schema, **options):
        self._schema = schema
        self._value = options.get('value') or None
        
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
        return str(option.value)
       
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
                #print 'Setting ' + str(opt) + ' value: ' + str(value)
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
        
        option_type.accept(self)
    
    def visit_StringOptionType(self, option_type):
        pass
            
    def visit_NumberOptionType(self, option_type):
        pass
    
    def visit_BooleanOptionType(self, option_type):
        pass
    
    def visit_FilenameOptionType(self, option_type):
        pass
    
    def visit_DatetimeOptionType(self, option_type):
        pass
    
    def visit_ColorOptionType(self, option_type):
        pass
    
    def visit_ChoiceOptionType(self, option_type):
        for option in option_type.options():
            opt = et.SubElement(self._option_elem, 'option')
            opt.attrib['value'] = option
            
    def visit_ManyOptionType(self, option_type):
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
        print "Unserializing section " + name
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
        
        print name + ":" + str(option_type)
        
        option = ConfigurationSchemaOption(name, option_type, documentation=doc)
        
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
        return option_type.accept(self)
    
    def visit_NumberOptionType(self, option_type):
        return option_type
    
    def visit_StringOptionType(self, option_type):
        return option_type
    
    def visit_BooleanOptionType(self, option_type):
        return option_type
    
    def visit_FilenameOptionType(self, option_type):
        return option_type
    
    def visit_ChoiceOptionType(self, option_type):
        for option in self._option_type_elem.iterchildren(tag='option'):
            option_type.add_option(option.attrib['value'])
        return option_type
    
    def visit_ManyOptionType(self, option_type):
        many_type_elem = self._option_type_elem.find('type')
        
        many_type = self.unserialize_option_type(many_type_elem)
        option_type.option_type = many_type
        return option_type
    
    def visit_DatetimeOptionType(self, option_type):
        return option_type
    
    def visit_ColorOptionType(self, option_type):
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
    
class DependencyExpressionParser():
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
        print "Option path " + str(ast)
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
                
        print "Identifier:" + name
        return name.strip()
    
    def literal_string(self, ast):
        string = ''
        for x in ast:
            string = string + x
        return DEString(string)
    
    def boolexp(self, ast):
        print "Boolexp: " + str(ast)
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
        print "Boolterm: " + str(ast)
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