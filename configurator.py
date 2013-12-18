import Tkinter as tk
import ttk
import widgets as w
import configuration as conf
import tkMessageBox
import tkColorChooser
import tkFileDialog
import os
import pytz # for timezones
import pycountry # for countries and languages
import tkCalendar
    
class ConfigurationSchemaNavigator(tk.Frame):
    def __init__(self, master, schemas):
        tk.Frame.__init__(self, master)
        
        self.schemas = schemas
        self.items = {}
        
        # The pane
        self.pane = tk.Frame(self)
        
        # The tree
        tr = tk.Frame(self.pane)
        self.tree = ttk.Treeview(tr)
        ysb = ttk.Scrollbar(tr, orient='vertical', command=self.tree.yview)
        ysb.pack(side=tk.RIGHT, fill=tk.Y)
        
        xsb = ttk.Scrollbar(tr, orient='horizontal', command=self.tree.xview)
        xsb.pack(side=tk.BOTTOM, fill=tk.X)
        
        self.tree.configure(yscroll=ysb.set, xscroll=xsb.set)
        #self.tree.heading('#0', text='Configuration schemas', anchor='w')
        
        self.tree.pack(fill=tk.Y)
        
        self.tree.bind('<Leave>', lambda ev:configurator.status.set(''))
                
        for schema in schemas:
            self.insert_schema(schema)
            
        # Popup menus
        self.tree.tag_bind('schema', '<ButtonRelease-1>', self.select_schema)
        #self.tree.tag_bind('schema', '<ButtonRelease-3>', self.popup_schema)
        
        self.tree.tag_bind('section', '<ButtonRelease-1>', self.select_section)
        #self.tree.tag_bind('section', '<ButtonRelease-3>', self.popup_section)
        
        self.tree.tag_bind('option', '<ButtonRelease-1>', self.select_option)
        #self.tree.tag_bind('option', '<ButtonRelease-3>', self.popup_option)
                        
        self.tree.bind('<ButtonRelease-3>', self.popup_tree)
        
        #Configuration
        #print(self.tree.tag_configure('schema'))
        #self.tree.tag_configure('schema', font=('Helvetica', '16'))
        
        self.tree.selection_set(self.tree.get_children()[0])
        
        tr.grid(column=0, row=0, sticky=tk.N+tk.S)
        
        # The editor
        self.editor = ConfigurationSchemaEditor(self.pane, schemas[0])
        self.editor.grid(column=1, row=0)
        self.pane.pack(fill=tk.BOTH, expand=True)
        
    def popup_tree(self, ev):
        
        item = self.tree.identify_row(ev.y)
        
        if not item:
            self.popup_noitem(ev)
        elif isinstance(self.items[item], conf.ConfigurationSchema):
            self.popup_schema(ev)
        elif isinstance(self.items[item],conf.ConfigurationSchemaSection):
            self.popup_section(ev)
        elif isinstance(self.items[item], conf.ConfigurationSchemaOption):
            self.popup_option(ev)
            
    def popup_noitem(self, ev):
        popup = tk.Menu(self, tearoff=0)
        popup.add_command(label="New configuration schema", command=self.create_schema) # , command=next) etc...
        popup.add_separator()
        popup.add_command(label="Dismiss")
            
        # display the popup menu
        try:
            popup.tk_popup(ev.x_root, ev.y_root, 0)
        finally:
            # make sure to release the grab (Tk 8.0a1 only)
            popup.grab_release()
                
    def select_schema(self, ev):
        item_id = str(self.tree.focus())
        schema = self.find_schema(item_id)
        print 'Selected schema was %s' % item_id
        print schema
        self.editor.grid_forget()
        self.editor = ConfigurationSchemaEditor(self.pane, schema, 
                                                onsave=lambda: self.tree.item(item_id, text=schema.name),
                                                onremove=lambda: self.tree.delete(item_id))
        self.editor.grid(column=1, row=0)
        
    def popup_schema(self, ev):
        # find the schema
        item = self.tree.identify_row(ev.y)
        schema = self.find_schema(item)
        
        # create a menu
        popup = tk.Menu(self, tearoff=0)
        popup.add_command(label="Remove", command=lambda: self.remove_schema(schema, item)) # , command=next) etc...
        popup.add_command(label="Add section", command=lambda: self.add_section(schema, item))
        popup.add_separator()
        popup.add_command(label="Dismiss")
        
        # display the popup menu
        try:
            popup.tk_popup(ev.x_root, ev.y_root, 0)
        finally:
            # make sure to release the grab (Tk 8.0a1 only)
            popup.grab_release()
    
    def schema_status(self, ev):
        item_id = str(self.tree.focus())
        schema = self.find_schema(item_id)
        set_status_message(self, "Edit " + schema.name + " configuration schema")
               
    def find_schema(self, id):
        return self.items[id]
    
    def create_schema(self):
        def save_schema(schema):
            self.schemas.append(schema)
            id = self.tree.insert('', 'end', text=schema.name, tags='schema')
            self.items[id] = schema
            configurator.status.set('Configuration schema ' + schema.name + ' has been created')
            
        creator = ConfigurationSchemaCreator(self, onsave=save_schema)
        self.wait_window(creator)

    def remove_schema(self, schema, id):
        if tkMessageBox.askquestion("Remove?", "Remove " + schema.name + " configuration schema?") == 'yes':
            schema.remove
            self.tree.delete(id)
            
            
    def select_section(self, ev):
        item_id = str(self.tree.focus())
        print 'Selected section was %s' % item_id
        section = self.find_section(item_id)
        print section
        self.editor.grid_forget()
        self.editor = ConfigurationSchemaSectionEditor(self.pane, section,
                                                       onsave=lambda:self.tree.item(item_id, text=section.name), 
                                                       onremove=lambda: self.tree.delete(item_id))
        self.editor.grid(column=1, row=0)
        
    def popup_section(self, ev):
        # find the section
        item = self.tree.identify_row(ev.y)
        section = self.find_section(item)
        
        # create a menu
        popup = tk.Menu(self, tearoff=0)
        popup.add_command(label="Remove", command=lambda:self.remove_section(section, item))
        popup.add_command(label="Add subsection", command=lambda:self.add_subsection(section, item))
        popup.add_command(label="Add option", command=lambda:self.add_option(section, item))
        popup.add_separator()
        popup.add_command(label="Dismiss")
        
        # display the popup menu
        try:
            popup.tk_popup(ev.x_root, ev.y_root, 0)
        finally:
            # make sure to release the grab (Tk 8.0a1 only)
            popup.grab_release()
            
    def section_status(self, ev):
        item_id = str(self.tree.focus())
        section = self.find_section(item_id)
        set_status_message(self, "Edit " + section.name + " section")
        
           
    def find_section(self, id):
        return self.items[id]
    
    def add_section(self, schema, item_id):
        def save_section(section):
            schema.section(section)
            id = self.tree.insert(item_id, 'end', text=section.name, tags='section')
            self.items[id] = section
            configurator.status.set('Section ' + section.name + ' created in ' + schema.name + ' configuration schema')
            
        creator = ConfigurationSchemaSectionCreator(self, onsave=save_section)
        self.wait_window(creator)
        
    def add_subsection(self, section, item_id):        
        def save_subsection(subsection):
            section.add_section(subsection)
            id = self.tree.insert(item_id, 'end', text=subsection.name, tags='section')
            self.items[id] = subsection
            configurator.status.set('Subsection ' + subsection.name + ' created in ' + section.name + ' section')
            
        creator = ConfigurationSchemaSectionCreator(self, onsave=save_subsection)
        self.wait_window(creator)
        
    def remove_section(self, section, id):
        if tkMessageBox.askquestion("Remove?", "Remove section " + section.name + "?") == 'yes':
            section.remove()
            self.tree.delete(id)
            del self.items[id]
            configurator.status.set(section.name + " section removed")
            
    def add_option(self, section, item_id):
        def save_option(option):
            section.add_option(option)
            id = self.tree.insert(item_id, 'end', text=option.name, tags='option')
            self.items[id] = option
            configurator.status.set('Option ' + option.name + ' created in ' + section.name + ' section')
            
        creator = ConfigurationSchemaOptionCreator(self.pane, onsave=save_option)
        self.wait_window(creator)        
    
    def select_option(self, ev):
        item_id = str(self.tree.focus())
        print 'Selected option was %s' % item_id
        option = self.find_option(item_id)
        print self.find_option(item_id)
        self.editor.grid_forget()
        self.editor = ConfigurationSchemaOptionEditor(self.pane, option, 
                                                      onsave=lambda:self.tree.item(item_id, text=option.name), 
                                                      onremove=lambda: self.tree.delete(item_id))
        self.editor.grid(column=1, row=0)
        
    def popup_option(self, ev):
        item = self.tree.identify_row(ev.y)
        option = self.find_option(item)
                 
        # create a menu
        popup = tk.Menu(self, tearoff=0)
        popup.add_command(label="Remove", command=lambda:self.remove_option(item, option)) # , command=next) etc...
        popup.add_separator()
        popup.add_command(label="Dismiss")
               
        # display the popup menu
        try:
            popup.tk_popup(ev.x_root, ev.y_root, 0)
        finally:
            # make sure to release the grab (Tk 8.0a1 only)
            popup.grab_release()
            
    def option_status(self, ev):
        item_id = str(self.tree.focus())
        option = self.find_option(item_id)
        set_status_message(self, "Edit " + option.name + " option")            
            
    def find_option(self, id):
        return self.items[id]
    
    def remove_option(self, id, option):
        if tkMessageBox.askquestion("Remove option", "Remove option " + option.name + "?") == 'yes':
            option.remove()
            self.tree.delete(id)        
        
    def insert_schema(self, schema):
        sc = self.tree.insert('', 'end', text=schema.name, tags='schema')
        self.items[sc] = schema
        for section in schema.sections():
            self.insert_section(section, sc)
            
    def insert_section(self, section, parent):
        sid = self.tree.insert(parent, 'end', text=section.name, tags='section')
        self.items[sid] = section
        
        for subsection in section.subsections():
            self.insert_section(subsection, sid)
        
        for option in section.options():
            oid = self.tree.insert(sid, 'end', text=option.name, tags='option')
            self.items[oid] = option
            
class ConfigurationSchemaEditor(tk.Frame):
    def __init__(self, master, schema, **options):
        tk.Frame.__init__(self, master)
        
        self.schema = schema
        self._onsave = options.get('onsave') or None
        self._onremove = options.get('onremove') or None
        
        # ui
        tk.Label(self, text=schema.name + " configuration schema").pack()
        props = tk.Frame(self)
        tk.Label(props, text="Name: ").grid(row=0, column=0, sticky=tk.W)
        self.schema_name = tk.StringVar()
        self.schema_name.set(schema.name or "")
        tk.Entry(props, textvariable=self.schema_name).grid(row=0, column=1, sticky=tk.W + tk.N)
        
        tk.Label(props, text="Parents:").grid(row=1, column=0, sticky=tk.W + tk.N)
        self.parents = w.DoubleListSelector(props, source=conf.list_configuration_schemas(),
                                        selected=schema.parents())
        set_status_message(self.parents, "Add and remove parents to the configuration schema")
        self.parents.grid(row=1, column=1, sticky=tk.W)
        
        tk.Label(props, text="Documentation:").grid(row=2, column=0, sticky=tk.W + tk.N)
        self.schema_doc = tk.Text(props, width=60, height=10)
        self.schema_doc.insert(tk.END, schema.documentation)
        self.schema_doc.grid(row=2, column=1, sticky=tk.W)
        
        buttons = tk.Frame(props)
        save = tk.Button(buttons, text="Save", command=self.save_schema)
        set_status_message(save, "Save changes to configuration schema")
        save.pack(side=tk.LEFT, padx=2)
        
        restore = tk.Button(buttons, text="Restore", command=self.restore_schema)
        set_status_message(restore, "Restore original configuration schema data")
        restore.pack(side=tk.LEFT, padx=2)
        
        remove = tk.Button(buttons, text="Remove", command=self.remove_schema)
        set_status_message(remove, "Remove the configuration schema")
        remove.pack(side=tk.LEFT, padx=2)
        
        buttons.grid(row=3, column=1, sticky=tk.SE)
        props.pack()
    
    def save_schema(self):
        self.schema.name = self.schema_name.get()
        self.schema.documentation = self.schema_doc.get(1.0, tk.END)
        #self.schema.set_parents(self.parents.get_selection())
        configurator.status.set(self.schema.name + " configuration schema has been updated")
        if self._onsave:
            self._onsave()
    
    def restore_schema(self):
        self.schema_name.set(self.schema.name)
        self.schema_doc.delete(1.0, tk.END)
        self.schema_doc.insert(1.0, self.schema.documentation)
        configurator.status.set(self.schema.name + " configuration schema has been restored to its original state")
        
    def remove_schema(self):
        if tkMessageBox.askquestion("Remove?", "Remove " + self.schema.name + " configuration schema?") == 'yes':
            self.schema.remove()
            configurator.status.set(self.schema.name + " configuration schema has been removed")
            if self._onremove:
                self._onremove()
                
class ConfigurationSchemaCreator(tk.Toplevel):
    def __init__(self, master, **options):
        tk.Toplevel.__init__(self, master)
                
        # configuration
        self._onsave = options.get('onsave') or None
            
        # ui
        self.title("New configuration schema")
        
        props = tk.Frame(self)
        tk.Label(props, text="Name: ").grid(row=0, column=0, sticky=tk.W)
        self.schema_name = tk.StringVar()
        tk.Entry(props, textvariable=self.schema_name).grid(row=0, column=1, sticky=tk.W + tk.N)
        
        tk.Label(props, text="Parents:").grid(row=1, column=0, sticky=tk.W + tk.N)
        self.parents = w.DoubleListSelector(props, source=conf.list_configuration_schemas(),
                                        selected=[])
        set_status_message(self.parents, "Add and remove parents to the configuration schema")
        self.parents.grid(row=1, column=1, sticky=tk.W)
        
        tk.Label(props, text="Documentation:").grid(row=2, column=0, sticky=tk.W + tk.N)
        self.schema_doc = tk.Text(props, width=60, height=10)
        self.schema_doc.grid(row=2, column=1, sticky=tk.W)
        
        buttons = tk.Frame(props)
        save = tk.Button(buttons, text="Save", command=self.save_schema)
        set_status_message(save, "Save changes to configuration schema")
        save.pack(side=tk.LEFT, padx=2)
        
        cancel = tk.Button(buttons, text="Cancel", command=self.destroy)
        cancel.pack(side=tk.LEFT, padx=2)
               
        buttons.grid(row=3, column=1, sticky=tk.SE)
        props.pack()
    
    def save_schema(self):
        schema = conf.ConfigurationSchema(self.schema_name.get())
         
        schema.documentation = self.schema_doc.get(1.0, tk.END)
        #schema.set_parents(self.parents.get_selection())
        
        configurator.status.set(schema.name + " configuration schema has been updated")
        
        if self._onsave:
            self._onsave(schema)
            
        self.destroy()
           
class ConfigurationSchemaSectionEditor(tk.Frame):
    def __init__(self, master, section, **options):
        tk.Frame.__init__(self, master)
        
        # configuration
        self.section = section
        self._onsave = options.get('onsave') or None
        self._onremove = options.get('onremove') or None
        
        # ui
        tk.Label(self, text=section.name + " section").pack()
        
        f = tk.Frame(self)
        
        tk.Label(f, text="Name: ").grid(row=0, column=0, sticky=tk.W)
        self.section_name = tk.StringVar()
        self.section_name.set(section.name or "")
        tk.Entry(f, textvariable=self.section_name).grid(row=0, column=1, sticky=tk.W)
        
        tk.Label(f, text="Documentation").grid(row=1, column=0, sticky=tk.W)
        self.section_documentation = tk.Text(f, height=10, width=60)
        self.section_documentation.insert(tk.END, self.section.documentation)
        self.section_documentation.grid(row=1, column=1, sticky=tk.W)
        
        buttons = tk.Frame(f)
        
        save = tk.Button(buttons, text="Save", command=self.save_section)
        save.pack(side=tk.LEFT, padx=2)
        set_status_message(save, "Update the section with the new data")
        
        restore = tk.Button(buttons, text="Restore", command=self.restore_section)
        restore.pack(side=tk.LEFT, padx=2)
        set_status_message(restore, "Restore the section data to its original form")
        
        remove = tk.Button(buttons, text="Remove", command=self.remove_section)
        remove.pack(side=tk.LEFT, padx=2)
        set_status_message(remove, "Remove the section")
        
        buttons.grid(row=2, column=1, sticky=tk.SE)
        
        f.pack()
        
    def save_section(self):
        self.section.name = self.section_name.get()
        self.section.documentation = self.section_documentation.get(1.0, tk.END)
        configurator.status.set(self.section.name + " section has been updated")
        
        if self._onsave:
            self._onsave()
            
    def restore_section(self):
        self.section_name.set(self.section.name)
        self.section_documentation.delete(1.0, tk.END)
        self.section_documentation.insert(1.0, self.section.documentation)
        configurator.status.set(self.section.name + " section has been restored to its original state")
                
    def remove_section(self):
        if tkMessageBox.askquestion("Remove?", "Remove section " + self.section.name + "?") == 'yes':
            self.section.remove()
            configurator.status.set(self.section.name + " section has been removed")
        
            if self._onremove:
                self._onremove()    
                
class ConfigurationSchemaSectionCreator(tk.Toplevel):
    def __init__(self, master, **options):
        tk.Toplevel.__init__(self, master)
        
        # configuration
        self.section = conf.ConfigurationSchemaSection()
        self._onsave = options.get('onsave') or None
        self.transient(master)
                
        # ui
        self.title("New section")
        
        f = tk.Frame(self)
        
        tk.Label(f, text="Name: ").grid(row=0, column=0, sticky=tk.W)
        self.section_name = tk.StringVar()
        tk.Entry(f, textvariable=self.section_name).grid(row=0, column=1, sticky=tk.W)
        
        tk.Label(f, text="Documentation").grid(row=1, column=0, sticky=tk.W)
        self.section_documentation = tk.Text(f, height=10, width=60)
        self.section_documentation.grid(row=1, column=1, sticky=tk.W)
        
        buttons = tk.Frame(f)
        
        save = tk.Button(buttons, text="Create", command=self.save_section)
        save.pack(side=tk.LEFT, padx=2)
        set_status_message(save, "Create the new section")
        
        cancel = tk.Button(buttons, text="Cancel", command=self.destroy)
        cancel.pack(side=tk.LEFT, padx=2)
               
        buttons.grid(row=2, column=1, sticky=tk.SE)
        
        f.pack()
        
    def save_section(self):
        self.section.name = self.section_name.get()
        self.section.documentation = self.section_documentation.get(1.0, tk.END)
        configurator.status.set(self.section.name + " section has been created")
        
        if self._onsave:
            self._onsave(self.section)
        self.destroy()
            
class ConfigurationSchemaOptionCreator(tk.Toplevel):
    def __init__(self, master, **options):
        tk.Toplevel.__init__(self, master)
        
        # configuration
        self.transient(master)
        self._onsave = options.get('onsave') or None
        self._onremove = options.get('onremove') or None
        
        # ui 
        self.title('New option')
        
        self.f = tk.Frame(self)
                
        # Name
        tk.Label(self.f, text="Name: ").grid(row=0, column=0, sticky=tk.W)
        self.option_name = tk.StringVar()
        tk.Entry(self.f, textvariable=self.option_name).grid(row=0, column=1, sticky=tk.W)
                
        # Option type
        tk.Label(self.f, text="Type: ").grid(row=1, column=0, sticky=tk.W)
        self.option_type = tk.StringVar()
        option_types = map(lambda o: o.option_name(), conf.OptionType.__subclasses__())
        options = tk.OptionMenu(self.f, self.option_type, option_types, command=self.edit_option_type)
        set_status_message(options, "Select the type of option")
        options.grid(row=1, column=1, sticky=tk.W) 
        
        self.option_type_editor = tk.Frame(self.f)
        self.option_type_editor.grid(row=2, column=1, sticky=tk.W)
        
        # Required?
        tk.Label(self.f, text="Is required?: ").grid(row=3, column=0, sticky=tk.W)
        self.option_required = tk.IntVar()
        self.option_required.set(1)
        
        required = tk.Checkbutton(self.f, variable=self.option_required)
        set_status_message(required, "Whether the option is required. If the option is required, then it is mandatory to set its value in the configuration")
        required.grid(row=3, column=1, sticky=tk.W)
        
        
        # Documentation
        tk.Label(self.f, text="Documentation:").grid(row=4, column=0, sticky=tk.W)
        self.option_documentation = tk.Text(self.f, width=60, height=10)
        self.option_documentation.grid(row=4, column=1, sticky=tk.W)
        
        buttons = tk.Frame(self.f)
    
        save = tk.Button(buttons, text="Create", command=self.save_option)
        set_status_message(save, "Create the new option")
        save.pack(side=tk.LEFT, padx=2)
        
        cancel = tk.Button(buttons, text="Cancel", command=self.destroy)
        cancel.pack(side=tk.LEFT, padx=2)
               
        buttons.grid(row=5, column=1, sticky=tk.SE)
        
        self.f.pack()
        
    def edit_option_type(self, ev):
        print "Edit option type" + self.option_type.get() 
        option_type = conf.OptionType.get_named(self.option_type.get())
        
        editor = OptionTypeEditor.for_option_type(option_type)
        print "Editor " + str(editor)
            
        self.option_type_editor.grid_forget()     
        
        if editor:
            self.option_type_editor = editor(self.f, option_type())
            self.option_type_editor.grid(row=2, column=1)
    
    def save_option(self):
        option_name = self.option_name.get()
        option_type = conf.OptionType.get_named(self.option_type.get())
        
        option = conf.ConfigurationSchemaOption(option_name, option_type())
        option.is_required=self.option_required.get() == 1
        option.documentation = self.option_documentation.get(1.0, tk.END)
        
        configurator.status.set(option.name + " option has been created")
        
        if self._onsave:
            self._onsave(option)
            
        self.destroy()
                      
class ConfigurationSchemaOptionEditor(tk.Frame):
    def __init__(self, master, option, **options):
        tk.Frame.__init__(self, master)
        
        # configuration
        self.option = option
        self._onsave = options.get('onsave') or None
        self._onremove = options.get('onremove') or None
        
        # ui 
        tk.Label(self, text=option.name + " option").pack()
        
        self.f = tk.Frame(self)
                
        # Name
        tk.Label(self.f, text="Name: ").grid(row=0, column=0, sticky=tk.W)
        self.option_name = tk.StringVar()
        self.option_name.set(option.name or "")
        tk.Entry(self.f, textvariable=self.option_name).grid(row=0, column=1, sticky=tk.W)
                
        # Option type
        tk.Label(self.f, text="Type: ").grid(row=1, column=0, sticky=tk.W)
        self.option_type = tk.StringVar()
        if option.option_type:
            self.option_type.set(option.option_type.name)
        option_types = map(lambda o: o.option_name(), filter(lambda ot: ot.name, conf.OptionType.__subclasses__()))
        options = tk.OptionMenu(self.f, self.option_type, *option_types, command=self.edit_option_type)
        set_status_message(options, "Select the type of option")
        options.grid(row=1, column=1, sticky=tk.W) 
        
        # Option type editor
        if option.option_type:
            editor = OptionTypeEditor.for_option_type(option.option_type.__class__)
            if editor:
                self.option_type_editor = editor(self.f, option.option_type)
            else:
                self.option_type_editor = tk.Frame(self.f)
        else:
            self.option_type_editor = tk.Frame(self.f)
            
        self.option_type_editor.grid(row=2, column=1, sticky=tk.W)
        
        # Default value
        tk.Label(self.f, text="Default value: ").grid(row=3, column=0, sticky=tk.W)
        
        self._default_value_var = tk.IntVar()
        if option.default_value:
            self._default_value_var.set(1)
        
        tk.Checkbutton(self.f, variable=self._default_value_var, command=self.set_default_value).grid(row=3, column=1, sticky=tk.W)
        
        if option.option_type and option.default_value:
            editor = OptionEditor.for_option_schema(option)
            self._default_value_editor = editor(self.f, option_schema=option)
        else:
            self._default_value_editor = tk.Frame(self.f)
            
        self._default_value_editor.grid(row=4, column=1, sticky=tk.W)       
        
        # Required?
        tk.Label(self.f, text="Is required?: ").grid(row=5, column=0, sticky=tk.W)
        self.option_required = tk.IntVar()
        self.option_required.set(1 if option.is_required else 0)
        required = tk.Checkbutton(self.f, variable=self.option_required)
        set_status_message(required, "Whether the option is required. If the option is required, then it is mandatory to set its value in the configuration")
        required.grid(row=5, column=1, sticky=tk.W)
                
        # Documentation
        tk.Label(self.f, text="Documentation:").grid(row=6, column=0, sticky=tk.W)
        self.option_documentation = tk.Text(self.f, width=60, height=10)
        self.option_documentation.insert(tk.END, self.option.documentation)
        self.option_documentation.grid(row=6, column=1, sticky=tk.W)
        
        buttons = tk.Frame(self.f)
    
        save = tk.Button(buttons, text="Save", command=self.save_option)
        set_status_message(save, "Save option changes")
        save.pack(side=tk.LEFT, padx=2)
        
        restore = tk.Button(buttons, text="Restore", command=self.restore_option)
        set_status_message(restore, "Restore option original data")
        restore.pack(side=tk.LEFT, padx=2)
        
        remove = tk.Button(buttons, text="Remove", command=self.remove_option)
        set_status_message(remove, "Remove the option")
        remove.pack(side=tk.LEFT, padx=2)
        
        buttons.grid(row=7, column=1, sticky=tk.SE)
        
        self.f.pack()
        
    def edit_option_type(self, ev):
        print "Edit option type" + self.option_type.get() 
        option_type = conf.OptionType.get_named(self.option_type.get())
        
        editor = OptionTypeEditor.for_option_type(option_type)
        print "Editor " + str(editor)
            
        self.option_type_editor.grid_forget()     
        
        if editor:
            self.option_type_editor = editor(self.f, option_type())
            self.option_type_editor.grid(row=2, column=1)
        
        # Clear the default value
        self._default_value_var.set(0)
        self.set_default_value()
            
    def set_default_value(self):
        self._default_value_editor.grid_forget()
        
        if self._default_value_var.get() == 1:
            option_type = conf.OptionType.get_named(self.option_type.get())
            
            editor = OptionEditor.for_option_type(option_type)
            self._default_value_editor = editor(self.f, option_type=option_type)
        else:
            self._default_value_editor = tk.Frame(self.f)
            
        self._default_value_editor.grid(row=4, column=1, sticky=tk.W) 
    
    def save_option(self):
        self.option.name = self.option_name.get()
        self.option.documentation = self.option_documentation.get(1.0, tk.END)
        option_type = conf.OptionType.get_named(self.option_type.get())
        self.option.option_type = option_type()
        self.option.is_required = self.option_required.get() == 1
        
        configurator.status.set(self.option.name + " option has been updated")
        
        if self._onsave:
            self._onsave()
            
    def restore_option(self):
        self.option_name.set(self.option.name)
        self.option_documentation.delete(1.0, tk.END)
        self.option_documentation.insert(1.0, self.option.documentation)
        configurator.status.set(self.option.name + " option has been restored to its original state")
        
    def remove_option(self):
        if tkMessageBox.askquestion("Remove?", "Remove " + self.option.name + " option?") == 'yes':
            self.option.remove()
            
            if self._onremove:
                self._onremove()
        
class OptionTypeEditor(object, tk.Frame):
    option_type = None
    
    def __init__(self, parent, option_type):
        tk.Frame.__init__(self, parent)
        self.option_type = option_type
        
    @classmethod
    def for_option_type(cls, option_type):
        subclasses = OptionTypeEditor.__subclasses__()
        return next((editor for editor in subclasses if editor.option_type == option_type), None)

class ChoiceOptionTypeEditor(OptionTypeEditor, w.ListEditor):
    option_type = conf.ChoiceOptionType
    
    def __init__(self, parent, option_type):
        OptionTypeEditor.__init__(self, parent, option_type)
        
        self.options_var = tk.StringVar()
        self.options_var.set(' '.join(option_type.options()))
        
        w.ListEditor.__init__(self, parent, listvar=self.options_var)
        set_status_message(self, "The possible option choices")
        
class ConfigurationNavigator(tk.Frame):
    def __init__(self, master, configs):
        tk.Frame.__init__(self, master)
        
        # Configuration
        self._items = {}
        self._configs = configs
                
        # ui
        self._left_panel = tk.Frame(self)
        
        self._selected_config = tk.StringVar()
                        
        self._configs_list = tk.Listbox(self._left_panel, exportselection=0)
        self._configs_list.bind('<ButtonRelease-1>', self.select_config)
        self._configs_list.bind('<ButtonRelease-3>', self.configs_popup)
        
        for config in configs:
            self._configs_list.insert(tk.END, config.name)
            
        self._configs_list.select_set(0)
            
        self._configs_list.pack()
        
        self._sections = ttk.Treeview(self._left_panel)
        self._sections.bind('<ButtonRelease-1>', self.select_section)
        self._sections.bind('<ButtonRelease-3>', self.sections_popup)
        
        ysb = ttk.Scrollbar(self._left_panel, orient='vertical', command=self._sections.yview)
        xsb = ttk.Scrollbar(self._left_panel, orient='horizontal', command=self._sections.xview)
        self._sections.configure(yscroll=ysb.set, xscroll=xsb.set)
        
        self._config = configs[0]
        sections = self._config.sections()
        
        for section in sections:
            self.insert_section(section)
            
        self._sections.selection_set(self._sections.get_children()[0])
        self._sections.pack()
        self._left_panel.pack(side=tk.LEFT)
           
        self._right_panel = tk.Frame(self, pady=10, relief=tk.FLAT)
        
        self.insert_section_editor(sections[0])
                           
        self._right_panel.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
                        
        self.pack()
        
    def insert_section_editor(self, section):
        row = 0
        
        for option in section.options():
            label = tk.Label(self._right_panel, text=option.name)
            
            # Option label popup
            label.bind('<ButtonRelease-3>', lambda ev: self.option_popup(ev, option))
            
            label.grid(row=row, column=0, padx=30, sticky=tk.W)
            
            #print "Option" + str(option.option_type)
            option_editor = OptionEditor.for_option_type(option.option_type.__class__)
            #print "Editor" + str(option_editor)
            option_value = self._config.option_value(option)
            if option_value:
                option_editor.set_value(option_value)
             
            option_editor(self._right_panel, option_schema=option).grid(row=row, column=1, padx=10, sticky=tk.W)
            
            tk.Label(self._right_panel, text=option.documentation).grid(row=row, column=2, padx=20, sticky=tk.W)
                
            row = row + 1
            
        buttons = tk.Frame(self._right_panel)
        
        save = tk.Button(buttons, text="Save", command=self.save_options)
        save.pack(side=tk.LEFT, padx=2)
        
        restore = tk.Button(buttons, text="Restore", command=self.restore_options)
        restore.pack(side=tk.LEFT, padx=2)
        
        buttons.grid(row=row, column=3, sticky=tk.SE)
        
    def select_config(self, ev):
        # Grab the selected configuration
        selection = self._configs_list.curselection()
        self._config = self._configs[int(selection[0])]
        
        # Refill the sections list with the configuration sections
        self._sections.delete(*self._sections.get_children())
        self._items = {}
        
        sections = self._config.sections()
        for section in sections:
            self.insert_section(section)
            
        self._sections.selection_set(self._sections.get_children()[0])
        
        # Clear the right panel
        self._right_panel.forget()
        self._right_panel = tk.Frame(self, pady=10, relief=tk.FLAT)
        
        # Put the options editing on the right panel
        self.insert_section_editor(sections[0])
                
        self._right_panel.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        
    def select_section(self, ev):
        id = self._sections.identify_row(ev.y)
        section = self._items[id]
        print "Section: " + section.name
        
        # Clear the right panel
        self._right_panel.forget()
        self._right_panel = tk.Frame(self, pady=10, relief=tk.FLAT)
        
        # Put the options editing on the right panel
        self.insert_section_editor(section)
        
        self._right_panel.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)               
        
    def insert_section(self, section, parent=''):
        sid = self._sections.insert(parent, 'end', text=section.name)
        self._items[sid] = section
        
        for subsection in section.subsections():
            self.insert_section(subsection, sid)
            
    def save_options(self):
        print "Save options"
        
    def restore_options(self):
        print "Restore options"
        
    def sections_popup(self, ev):
        print "Sections popup"
        item = self._sections.identify_row(ev.y)
        section = self._items[item]
        print "Section: " +  section.name
                 
    def configs_popup(self, ev):
        print "Configs popup"
        index = self._configs_list.nearest(ev.y)
        
        # create a menu
        popup = tk.Menu(self, tearoff=0)
        
        _, yoffset, _, height = self._configs_list.bbox(index)
        if ev.y > height + yoffset + 5: # XXX 5 is a niceness factor :)
            # Outside of widget.
            popup.add_command(label="Add configuration", command=self.create_config)
        else:
        
            config = self._configs_list.get(index)
            print "Config: " + config
                    
            popup.add_command(label="Remove", command=lambda:self.remove_config(config, index))
            popup.add_command(label="Edit", command=lambda:self.edit_config(config, index))
            
        popup.add_separator()
        popup.add_command(label="Dismiss")
        
        # display the popup menu
        try:
            popup.tk_popup(ev.x_root, ev.y_root, 0)
        finally:
            # make sure to release the grab (Tk 8.0a1 only)
            popup.grab_release()
    
    def create_config(self):
        print "Create config"
        
    def edit_config(self, config, index):
        print "Edit config " + str(config)
        
    def remove_config(self, config, index):
        print "Remove config " + str(config)
        
    def option_popup(self, ev, option):
        # create a menu
        popup = tk.Menu(self, tearoff=0)
        
        popup.add_command(label="Unset", command=lambda:self.unset_option(option))
        popup.add_command(label="Restore", command=lambda:self.restore_option(option))
            
        popup.add_separator()
        popup.add_command(label="Dismiss")
        
        # display the popup menu
        try:
            popup.tk_popup(ev.x_root, ev.y_root, 0)
        finally:
            # make sure to release the grab (Tk 8.0a1 only)
            popup.grab_release()
            
    def unset_option(self, option):
        print "Unset option " + str(option)
        
    def restore_option(self, option):
        print "Restore option " + str(option)        
                
class AboutDialog(tk.Toplevel):

    def __init__(self, parent):
        
        tk.Toplevel.__init__(self, parent)

        self.transient(parent)
        self.title("About configurator")
        
        self.geometry("+%d+%d" % (parent.winfo_rootx()+50,
                                  parent.winfo_rooty()+50))
        
        logo = tk.PhotoImage(file=image('system-settings-2.gif'))
        
        label = tk.Label(self,image=logo)
        label.image = logo # avoid garbage collection
        label.pack()
        
        tk.Label(self, text="This is configurator, a tool for managing application configurations." +
                            "\n\n Home page: https://github.com/mmontone/configurator" +  
                            "\n\n Author: Mariano Montone").pack()

        b = tk.Button(self, text="OK", command=self.ok)
        b.pack(pady=5)

    def ok(self):

        self.destroy()
        
class OptionEditor(object, tk.Frame):
    def __init__(self, master, **args):
        tk.Frame.__init__(self, master)
        
        self._option_schema = None
        self._option_schema = args.get('option_schema') or None
        if args.get('option'):
            self._option_schema = args.get('option').schema
                
    @classmethod
    def for_option(cls, option):
        return cls.for_option_schema(option.schema)
    
    @classmethod
    def for_option_schema(cls, option_schema):
        return cls.for_option_type(option_schema.option_type)
       
    @classmethod
    def for_option_type(cls, option_type):
        subclasses = OptionEditor.__subclasses__()
        return next((editor for editor in subclasses if editor.option_type == option_type), None)
        
class StringOptionEditor(OptionEditor):
    option_type = conf.StringOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        self._var = tk.StringVar()
        if self._option_schema and self._option_schema.default_value:
            self._var.set(self._option_schema.default_value)
            
        entry = tk.Entry(self, textvariable=self._var)
        entry.pack()
        
    def value(self):
        return self._var.get()
        
class NumberOptionEditor(OptionEditor):
    option_type = conf.NumberOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
                
        self._var = tk.StringVar()
        if self._option_schema and self._option_schema.default_value:
            self._var.set(str(self._option_schema.default_value))
        
        sb = tk.Spinbox(self, textvariable=self._var)
        sb.pack()
        
    def value(self):
        return int(self._var.get())
        
class BooleanOptionEditor(OptionEditor):
    option_type = conf.BooleanOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
                
        self._var = tk.IntVar()
        if self._option_schema and self._option_schema.default_value:
            self._var.set(1)
                        
        cb = tk.Checkbutton(self, variable=self._var)
        cb.pack()
        
class ChoiceOptionEditor(OptionEditor):
    option_type = conf.ChoiceOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        #lb = tk.Listbox(self)
        
        #for option in self._option_schema.options():
        #    lb.insert(tk.END, option)
            
        #lb.pack()
        
    def value(self):
        return self._var.get()
    
class TimezoneOptionEditor(OptionEditor):
    option_type = conf.TimezoneOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        lb = tk.Listbox(self)
        
        for tz in pytz.all_timezones:
            lb.insert(tk.END, str(tz))
            
        ysb = ttk.Scrollbar(self, orient='vertical', command=lb.yview)
        lb.configure(yscroll=ysb.set)
        lb.pack(side=tk.LEFT)
        ysb.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        
class CountryOptionEditor(OptionEditor):
    option_type = conf.CountryOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        lb = tk.Listbox(self)
        
        for country in pycountry.countries:
            lb.insert(tk.END, country.name)
            
        ysb = ttk.Scrollbar(self, orient='vertical', command=lb.yview)
        lb.configure(yscroll=ysb.set)
        lb.pack(side=tk.LEFT)
        ysb.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        
class LanguageOptionEditor(OptionEditor):
    option_type = conf.LanguageOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        lb = tk.Listbox(self)
        
        for lang in pycountry.languages:
            lb.insert(tk.END, lang.name)
            
        ysb = ttk.Scrollbar(self, orient='vertical', command=lb.yview)
        lb.configure(yscroll=ysb.set)
        lb.pack(side=tk.LEFT)
        ysb.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        
class CurrencyOptionEditor(OptionEditor):
    option_type = conf.CurrencyOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        lb = tk.Listbox(self)
        
        for currency in pycountry.currencies:
            lb.insert(tk.END, currency.name)
            
        ysb = ttk.Scrollbar(self, orient='vertical', command=lb.yview)
        lb.configure(yscroll=ysb.set)
        lb.pack(side=tk.LEFT)
        ysb.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)                
    
class ColorOptionEditor(OptionEditor):
    option_type = conf.ColorOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        self._var = tk.StringVar()
        self._var.set('No color selected')
        
        tk.Label(self, textvariable=self._var).pack()
        tk.Button(self, text='Select Color', command=self.getColor).pack()
        
    def getColor(self):
        color = tkColorChooser.askcolor()
        self._var.set(color)
        
class FilenameOptionEditor(OptionEditor):
    option_type = conf.FilenameOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        self._var = tk.StringVar()
            
        tk.Entry(self, textvariable=self._var).pack()
        tk.Button(self, text='Select file', command=self.getFilename).pack()

    def getFilename(self):
        filename = tkFileDialog.askopenfilename()
        self._var.set(filename)
        
class DirectoryOptionEditor(OptionEditor):
    option_type = conf.DirectoryOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        self._var = tk.StringVar()
        
        tk.Entry(self, textvariable=self._var).pack()
        tk.Button(self, text='Select directory', command=self.getDirectory).pack()

    def getDirectory(self):
        directory = tkFileDialog.askdirectory()
        self._var.set(directory)
        
class URIOptionEditor(OptionEditor):
    option_type = conf.URIOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        self._var = tk.StringVar()
        if self._option_schema and self._option_schema.default_value:
            self._var.set(self._option_schema.default_value)
            
        entry = tk.Entry(self, textvariable=self._var)
        entry.pack()
        
    def value(self):
        return self._var.get()
    
class TimeOptionEditor(OptionEditor):
    option_type = conf.TimeOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        self._hours_var = tk.IntVar()
        self._minutes_var = tk.IntVar()
        self._seconds_var = tk.IntVar()
        
        self._hours = tk.Spinbox(self, from_=0, to=23)
        self._hours.pack(side=tk.LEFT)
        
        self._minutes = tk.Spinbox(self, from_=0, to=59)
        self._minutes.pack(side=tk.LEFT)
        
        self._seconds = tk.Spinbox(self, from_=0, to=59)
        self._seconds.pack(side=tk.LEFT)
        
class DateOptionEditor(OptionEditor):
    option_type = conf.DateOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        tkCalendar.Calendar(self,date="21/11/2006",dateformat="%d/%m/%Y").pack()
       
        
class DatetimeOptionEditor(OptionEditor):
    option_type = conf.DatetimeOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        tkCalendar.Calendar(self,date="21/11/2006",dateformat="%d/%m/%Y").pack()
        
        time = tk.Frame(self)
        
        self._hours_var = tk.IntVar()
        self._minutes_var = tk.IntVar()
        self._seconds_var = tk.IntVar()
        
        self._hours = tk.Spinbox(time, from_=0, to=23)
        self._hours.pack(side=tk.LEFT)
        
        self._minutes = tk.Spinbox(time, from_=0, to=59)
        self._minutes.pack(side=tk.LEFT)
        
        self._seconds = tk.Spinbox(time, from_=0, to=59)
        self._seconds.pack(side=tk.LEFT)
        time.pack()
          
class Configurator(tk.Frame):
    def __init__(self, parent):
        
        tk.Frame.__init__(self, parent, relief=tk.SUNKEN)
        
        parent.title('Configurator')
        
        # Menubar
        self.menu_bar = tk.Menu(self)
        
        help_menu = tk.Menu(self.menu_bar)
        help_menu.add_command(label='About', command=self.help_about)
        set_status_message(help_menu, 'About configurator')
        
        self.menu_bar.add_cascade(label='Help', menu=help_menu)
        
        #quit_icon = tk.PhotoImage(file="/home/marian/workspace2/configurator/images/application-exit-2.gif")
        #self.menu_bar.add_command(label='Quit', image=quit_icon, compound=tk.RIGHT, command=self.quit)
        #self.menu_bar.icon = quit_icon
        
        self.menu_bar.add_command(label='Quit', command=self.quit)
                
        try:
            parent.config(menu=self.menu_bar)
        except AttributeError:
            # master is a toplevel window (Python 1.4/Tkinter 1.63)
            parent.tk.call(parent, "config", "-menu", menu_bar)
            
        # Tabs
        tabs = ttk.Notebook(self, name='notebook')
        tabs.enable_traversal()
        
        navigator = self.init_schemas_navigator()
        tabs.add(navigator, text='Configuration schemas')
        
        configs_nav = self.init_configs_navigator()
        tabs.add(configs_nav, text='Configurations')
                      
        tabs.pack(fill=tk.BOTH, expand=tk.Y, padx=2, pady=3)
        
        # Status bar
        self.status = w.StatusBar(self)
        self.status.pack(side=tk.BOTTOM, fill=tk.X)
        
    def init_schemas_navigator(self):
        self._schemas = {}
        
        sch1 = conf.ConfigurationSchema("Web")
        s1 = conf.ConfigurationSchemaSection("Server")
        sch1.section(s1)
        
        host = conf.ConfigurationSchemaOption("Host", conf.StringOptionType(), documentation="Server host")
        s1.add_option(host)
        
        port = conf.ConfigurationSchemaOption("Port", conf.NumberOptionType(), documentation="Port number")
        s1.add_option(port)
        
        s2 = conf.ConfigurationSchemaSection("Authentication")
        sch1.section(s2)
        
        auth = conf.ConfigurationSchemaOption('Authentication enabled', conf.BooleanOptionType(), documentation='Enable authentication?')
        s2.add_option(auth)
        
        s3 = conf.ConfigurationSchemaSection("Logging")
        sch1.section(s3)
        
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
        
        self._schemas[sch1.name] = sch1
    
        db = conf.ConfigurationSchema("Database")
        db_engine = conf.ConfigurationSchemaOption("engine", 
                                                   conf.ChoiceOptionType(["Postgresql", "Mysql"]), 
                                                   documentation="The database engine")
        db_engine.is_required = True
        db_server = conf.ConfigurationSchemaSection("Server").add_option(db_engine)
        db.section(db_server)
        self._schemas[db.name] = db
                
        return ConfigurationSchemaNavigator(self, self._schemas.values())
    
    def init_configs_navigator(self):
        dev = conf.Configuration('Dev', self._schemas['Web'])
        test = conf.Configuration('Test', self._schemas['Web'])
        prod = conf.Configuration('Prod', self._schemas['Database'])
        
        return ConfigurationNavigator(self, [dev, test,prod])                        
       
    def help_about(self):
        d = AboutDialog(self)

        self.wait_window(d)
        
    def quit(self):
        root.quit()
        
def set_status_message(widget, message):
    global configurator
    widget.bind('<Enter>', lambda ev:configurator.status.set(message))
    widget.bind('<Leave>', lambda ev:configurator.status.set(''))
    
def image(filename):
    return os.path.dirname(os.path.realpath(__file__)) + "/images/" + filename
        
if __name__ == '__main__':
    root = tk.Tk()
    configurator = Configurator(root)
    configurator.pack(fill=tk.BOTH, expand=True)
    root.mainloop()