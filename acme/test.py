import configuration as conf

def test_schemas():
        schemas = []
        
        sch1 = conf.ConfigurationSchema("Web")
        s1 = conf.ConfigurationSchemaSection("Web server")
        sch1.section(s1)
        
        host = conf.ConfigurationSchemaOption("Host", conf.StringOptionType(), documentation="Server host")
        host.default_value = 'http://localhost'
        s1.add_option(host)
        
        port = conf.ConfigurationSchemaOption("Port", conf.NumberOptionType(), documentation="Port number")
        port.default_value = 8080
        s1.add_option(port)
        
        s2 = conf.ConfigurationSchemaSection("Authentication")
        sch1.section(s2)
        
        auth = conf.ConfigurationSchemaOption('Authentication enabled', conf.BooleanOptionType(), documentation='Enable authentication?')
        auth.is_required=False
        s2.add_option(auth)
        
        sch_log = conf.ConfigurationSchema("Log")
        s3 = conf.ConfigurationSchemaSection("Logging")
        sch_log.section(s3)
        
        logfile = conf.ConfigurationSchemaOption('Logfile', conf.FilenameOptionType(), documentation='Where the logging happens')
        s3.add_option(logfile)
        
        datetime = conf.ConfigurationSchemaOption('Expire', conf.DatetimeOptionType(), documentation='Expiration')
        s3.add_option(datetime)
        
        s4 = conf.ConfigurationSchemaSection("General preferences")
        sch1.section(s4)
        
        fontsize = conf.ConfigurationSchemaOption('Font size', conf.NumberOptionType(), documentation='Font size')
        s4.add_option(fontsize)
        
        s5 = conf.ConfigurationSchemaSection('Colors')
        s4.add_section(s5)
        
        color = conf.ConfigurationSchemaOption('Background color', conf.ColorOptionType(), documentation='Background color')
        s5.add_option(color)        
        
        schemas.append(sch1)
    
        db = conf.ConfigurationSchema("Database")
        db_engine = conf.ConfigurationSchemaOption("engine", 
                                                   conf.ChoiceOptionType(["Postgresql", "Mysql"]), 
                                                   documentation="The database engine")
        db_engine.is_required = True
        db_server = conf.ConfigurationSchemaSection("Database server").add_option(db_engine)
        db.section(db_server)
        
        schemas.append(db)
        schemas.append(sch_log)
        
        app_sch = conf.ConfigurationSchema('App')
        app_sch.add_parent(db)
        app_sch.add_parent(sch1)
        app_sch.add_parent(sch_log)
        
        schemas.append(app_sch)
        
        return schemas
    
def test_configs():
    test_schemas()
    
    dev = conf.Configuration('Dev', conf.ConfigurationSchema.get_named('App'))
    test = conf.Configuration('Test', conf.ConfigurationSchema.get_named('App'))
    test.parent = dev
    prod = conf.Configuration('Prod', conf.ConfigurationSchema.get_named('Web'))
    
    return [dev, prod, test]