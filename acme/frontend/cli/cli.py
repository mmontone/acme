import configuration as conf
import os
from util import *

def create_config_from_schema(schema):
    for section in schema.sections():
        print section.name
        print section.description
