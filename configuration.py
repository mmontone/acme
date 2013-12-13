configuration_schemas = {}

def get_configuration_schema(name):
    global configuration_schemas
    return configuration_schemas[name]

def register_configuration_schema(schema):
    global configuration_schemas
    configuration_schemas[schema.name] = schema
    
def unregister_configuration_schema(schema):
    global configuration_schemas
    del configuration_schemas[schema.name]
    
def list_configuration_schemas():
    global configuration_schemas
    return configuration_schemas.values()

class ConfigurationSchema:
    def __init__(self, name, **args):
        self._name = name
        self._sections = {}
        self._documentation = args.get('documentation') or "Not documented"
        self._parents = args.get("parents") or []
        register_configuration_schema(self)
                
    def section(self, section):
        self._sections[section.name] = section
        section.schema = self
        return self
    
    def sections(self):
        return self._sections.values()
    
    def get_section(self, name):
        return self._sections[name]
    
    def remove_section(self, section):
        del self._sections[section.name]
    
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
        
class ConfigurationSchemaSection:
    def __init__(self, name, **args):
        self._name = name
        self._subsections = {}
        self._options = {}
        self._documentation = args.get('documentation') or 'Not documented'
        self._schema = args.get('schema') or None
        
    @property
    def name(self):
        return self._name
    
    @name.setter
    def name(self, value):
        self._name = value
        return self
        
    def subsections(self):
        return self._subsections.values()
            
    def add_section(self, section):
        self._subsections[section.name] = section
        return self
    
    def options(self):
        return self._options.values()
    
    def add_option(self, option):
        self._options[option.name] = option
        option.section = self
        return self
    
    def remove_option(self, option):
        del self._options[option.name]        
        
    def get_option(self, name):
        return self._options[name]
        
    @property
    def documentation(self):
        return self._documentation
    
    @documentation.setter
    def documentation(self, value):
        self._documentation = value
        return self
    
    @property
    def schema(self):
        return self._schema
    
    @schema.setter
    def schema(self, value):
        self._schema = value
        return self
    
    def remove(self):
        self.schema.remove_section(self)

class ConfigurationSchemaOption:
    def __init__(self, name, option_type, **args):
        self._name = name
        self._option_type = option_type
        self._documentation = args.get('documentation') or 'Not documented'
        self._section = None
        self._is_required = args.get('required') or True
    
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
    
class OptionType(object):
    _name = "Option type"
    
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
    
class PathnameOptionType(OptionType):
    _name = "Pathname"
    
class ColorOptionType(OptionType):
    _name = "Color"
    
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