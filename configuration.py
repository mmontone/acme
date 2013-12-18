configuration_schemas = []

def get_configuration_schema(name):
    global configuration_schemas
    return next((schema for schema in configuration_schemas if schema.name == name), None)

def register_configuration_schema(schema):
    global configuration_schemas
    configuration_schemas.append(schema)
    
def unregister_configuration_schema(schema):
    global configuration_schemas
    configuration_schemas.remove(schema)
    
def list_configuration_schemas():
    global configuration_schemas
    return configuration_schemas

class ConfigurationSchema:
    def __init__(self, name='', **args):
        self._name = name
        self._sections = []
        self._documentation = args.get('documentation') or "Not documented"
        self._parents = args.get("parents") or []
        register_configuration_schema(self)
                
    def section(self, section):
        self._sections.append(section)
        section.schema = self
        return self
    
    def sections(self):
        return self._sections
    
    def get_section(self, name):
        return next((s for s in self._sections if s.name == name), None)
        
    def remove_section(self, section):
        self._sections.remove(section)
            
    def parents(self):
        return self._parents
    
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
        unregister_configuration_schema(self)
    
    def __str__(self):
        return self.name 
    
    def path(self):
        return self.name
        
class ConfigurationSchemaSection:
    def __init__(self, name='', **args):
        self._name = name
        self._subsections = []
        self._options = []
        self._documentation = args.get('documentation') or 'Not documented'
        self._parent = args.get('parent') or None
        
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
    
    def remove(self):
        self.parent.remove_section(self)
        
    def path(self):
        return self._parent.path() + self.name

class ConfigurationSchemaOption:
    def __init__(self, name, option_type, **args):
        self._name = name
        self._option_type = option_type
        self._documentation = args.get('documentation') or 'Not documented'
        self._section = None
        self._is_required = args.get('required') or True
        self._default_value = args.get('default_value') or None
    
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
    
    def path(self):
        return self.section.path() + self.name
    
class OptionType(object):
    _name = None
    
    @classmethod
    def option_name(cls):
        return cls._name
    
    @classmethod
    def get_named(cls, name):
        return next((option_type for option_type in cls.__subclasses__() if option_type.option_name() == name), None)
    
    @property
    def name(self):
        return self.__class__.option_name()
   
class StringOptionType(OptionType):
    _name = "String"
    
class NumberOptionType(OptionType):
    _name = "Number"
    
class BooleanOptionType(OptionType):
    _name = "Boolean"
    
class EmailOptionType(OptionType):
    _name = "Email"
    
class URIOptionType(OptionType):
    _name = "URI"
    
class FilenameOptionType(OptionType):
    _name = "Filename"
    
class DirectoryOptionType(OptionType):
    _name = "Directory"
    
class ColorOptionType(OptionType):
    _name = "Color"
    
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
    
class ChoiceOptionType(OptionType):
    _name = "Choice"
    
    def __init__(self, options=[]):
        self._options = options
        
    def options(self):
        return self._options
    
class ListOptionType(OptionType):
    _name = "List"
    
    def __init__(self, options=[]):
        self._options = options
        
    def options():
        return self._options
    
class CompoundOptionType():
    def __init__(self, *option_types):
        self._option_types = option_types
        
    @property
    def option_types(self):
        return self._option_types 
    
    @option_types.setter
    def option_types(self, value):
        self._option_types = value
        return self
        
class OneOfOptionType(OptionType, CompoundOptionType):
    _name = "One of"
    
    def __init__(self, *option_types):
        OptionType.__init__(self, *option_types)
       
    
class ManyOptionType(OptionType, CompoundOptionType):
    _name = "Many"   
    
    def __init__(self, *option_types):
        OptionType.__init__(self, *option_types)
    
class Configuration():
    
    def __init__(self, name='', schema='', **options):
        self._name = name
        self._schema = schema
        self._parent = options.get('parent') or None
        self._options = {}
        
    @property
    def name(self):
        return self._name
    
    @name.setter
    def name(self, value):
        self._name = value
        return self
    
    @property
    def schema(self):
        return self._schema
    
    @schema.setter
    def schema(self, value):
        self._schema = value
        return self
    
    @property
    def parent(self):
        return self._parent
    
    @parent.setter
    def parent(self, value):
        self._parent = value
        return self
    
    def set_option_value(self, schema_option, value):
        option = ConfigurationOption(schema_option, value=value)
        self._options[schema_option] = option
        
    def option_value(self, schema_option):
        option = self._options.get(schema_option, None)
        if option:
            return option.value
        
    def sections(self):
        return self._schema.sections()
        
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
    def value(self):
        return self._value
    
    @value.setter
    def value(self, value):
        self._value = value
        return self
    
    def path(self):
        return self.schema.path()
        
    def __hash__(self):
        return hash(self.path())

    def __eq__(self, other):
        return self.path() == other.path()