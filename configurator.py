import Tkinter as tk
import ttk
import widgets as w
import configuration as conf
import tkMessageBox
import tkColorChooser
import tkFileDialog
import os
import sys
import pytz # for timezones
import pycountry # for countries and languages
import tkCalendar
import test
import argparse
import grako

configs_file = None
schemas_file = None
    
class ConfigurationSchemaNavigator(tk.Frame):
    def __init__(self, master, schemas):
        tk.Frame.__init__(self, master)
        
        self.schemas = schemas
        self.items = {}
        
        # The pane
        left_pane = tk.Frame(self)
                
        # The tree
        self.tree = ttk.Treeview(left_pane)
        ysb = ttk.Scrollbar(left_pane, orient='vertical', command=self.tree.yview)
        ysb.pack(side=tk.RIGHT, fill=tk.Y)
        
        xsb = ttk.Scrollbar(left_pane, orient='horizontal', command=self.tree.xview)
        xsb.pack(side=tk.BOTTOM, fill=tk.X)
        
        self.tree.configure(yscroll=ysb.set, xscroll=xsb.set)
        #self.tree.heading('#0', text='Configuration schemas', anchor='w')
        
        self.tree.pack(fill=tk.Y, expand=True)
        
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
        
        if len(self.tree.get_children()) > 0:
            self.tree.selection_set(self.tree.get_children()[0])
        
            # The editor
            self.editor = ConfigurationSchemaEditor(self, schemas[0])
        else:
            self.editor = tk.Frame(self)
            
        left_pane.pack(side=tk.LEFT, fill=tk.Y)
        self.editor.pack(side=tk.LEFT, fill=tk.BOTH, expand=True) 
                
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
        popup.add_command(label="New configuration schema", command=self.create_schema)
        popup.add_command(label="Save schemas", command=self.save_schemas)
        popup.add_command(label="Load schemas", command=self.load_schemas)
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
        self.editor.forget()
        self.editor = ConfigurationSchemaEditor(self, schema, 
                                                onsave=lambda: self.tree.item(item_id, text=schema.name),
                                                onremove=lambda: self.tree.delete(item_id))
        self.editor.pack(expand=True, fill=tk.BOTH)
        
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
            del self.items[id]            
            
    def select_section(self, ev):
        item_id = str(self.tree.focus())
        print 'Selected section was %s' % item_id
        section = self.find_section(item_id)
        print section
        self.editor.forget()
        self.editor = ConfigurationSchemaSectionEditor(self, section,
                                                       onsave=lambda:self.tree.item(item_id, text=section.name), 
                                                       onremove=lambda: self.tree.delete(item_id))
        self.editor.pack(fill=tk.BOTH, expand=True)
        
    def popup_section(self, ev):
        # find the section
        item = self.tree.identify_row(ev.y)
        section = self.find_section(item)
        
        # create a menu
        popup = tk.Menu(self, tearoff=0)
        popup.add_command(label="Add subsection", command=lambda:self.add_subsection(section, item))
        popup.add_command(label="Add option", command=lambda:self.add_option(section, item))
        popup.add_separator()
        popup.add_command(label="Move up", command=lambda:self.move_up_section(section))
        popup.add_command(label="Move down", command=lambda:self.move_down_section(section))
        popup.add_command(label="Remove", command=lambda:self.remove_section(section, item))
        popup.add_separator()
        popup.add_command(label="Dismiss")
        
        # display the popup menu
        try:
            popup.tk_popup(ev.x_root, ev.y_root, 0)
        finally:
            # make sure to release the grab (Tk 8.0a1 only)
            popup.grab_release()
            
    def move_up_section(self, section):
        print "Move up section " + str(section)
        
    def move_down_section(self, section):
        print "Move down section " + str(section)
            
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
            
        creator = ConfigurationSchemaOptionCreator(self, onsave=save_option)
        self.wait_window(creator)        
    
    def select_option(self, ev):
        item_id = str(self.tree.focus())
        print 'Selected option was %s' % item_id
        option = self.find_option(item_id)
        print self.find_option(item_id)
        self.editor.forget()
        self.editor = ConfigurationSchemaOptionEditor(self, option, 
                                                      onsave=lambda:self.tree.item(item_id, text=option.name), 
                                                      onremove=lambda: self.tree.delete(item_id))
        self.editor.pack(fill=tk.BOTH, expand=True)
        
    def popup_option(self, ev):
        item = self.tree.identify_row(ev.y)
        option = self.find_option(item)
                 
        # create a menu
        popup = tk.Menu(self, tearoff=0)
        popup.add_command(label="Move up", command=lambda:self.move_up_option(option))
        popup.add_command(label="Move down", command=lambda:self.move_down_option(option))
        popup.add_command(label="Remove", command=lambda:self.remove_option(item, option)) # , command=next) etc...
        popup.add_separator()
        popup.add_command(label="Dismiss")
               
        # display the popup menu
        try:
            popup.tk_popup(ev.x_root, ev.y_root, 0)
        finally:
            # make sure to release the grab (Tk 8.0a1 only)
            popup.grab_release()
            
    def move_up_option(self, option):
        print "Move up option " + str(option)
        
    def move_down_option(self, option):
        print "Move down option " + str(option)
            
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
        for section in schema.direct_sections():
            self.insert_section(section, sc)
            
    def insert_section(self, section, parent):
        sid = self.tree.insert(parent, 'end', text=section.name, tags='section')
        self.items[sid] = section
        
        for subsection in section.subsections():
            self.insert_section(subsection, sid)
        
        for option in section.options():
            oid = self.tree.insert(sid, 'end', text=option.name, tags='option')
            self.items[oid] = option
    
    def save_schemas(self):
        def save_schemas(schemas, filename, format):
            # Serialize the schemas
            serializer = conf.ConfigurationSchemasXMLSerializer()
            for schema in schemas:
                serializer.serialize(schema)
            print filename
            serializer.write(filename)
            tkMessageBox.showinfo('Schemas saved successfully', 'Schemas have been saved to ' + filename)
            
        dialog = SaveSchemasDialog(self, self.schemas, onsave=save_schemas)
        self.wait_window(dialog)
        
    def load_schemas(self):
        def load_schemas(schemas):
            print "Load schemas"
            
            self.tree.delete(*self.tree.get_children())
            self.items = {}
            self.editor.grid_forget()
            
            for schema in schemas:
                self.insert_schema(schema)
                
            self.tree.selection_set(self.tree.get_children()[0])
        
            self.editor = ConfigurationSchemaEditor(self, schemas[0])
            self.editor.pack(fill=tk.BOTH, expand=True)
                
        dialog = LoadSchemasDialog(self, onload=load_schemas)
        self.wait_window(dialog)
        
class SaveSchemasDialog(tk.Toplevel):
    
    def __init__(self, master, schemas, **options):
        tk.Toplevel.__init__(self, master)
        
        global schemas_file
        
        self._schemas = schemas
        self._onsave = options.get('onsave') or None
        
        self.title('Save schemas')
        
        tk.Label(self, text='Save to: ').grid(row=0, column=0, sticky=tk.NW)
        
        self._filename = tk.StringVar()
        self._filename.set(schemas_file)
        tk.Entry(self, textvariable=self._filename).grid(row=0, column=1, sticky=tk.NW)
        tk.Button(self, text="Select file", command=self.get_filename).grid(row=0, column=3, sticky=tk.NW)       
        
        tk.Label(self, text='Format: ').grid(row=1, column=0, sticky=tk.NW)
        
        self._format = tk.StringVar()
        self._format.set('xml')
               
        tk.OptionMenu(self, self._format, 'xml', 'yaml').grid(row=1, column=1, sticky=tk.NW)
        
        buttons = tk.Frame(self)
        save = tk.Button(buttons, text="Save", command=self.save_schemas)
        save.pack(side=tk.LEFT, padx=2)
        
        cancel = tk.Button(buttons, text="Cancel", command=self.destroy)
        cancel.pack(side=tk.LEFT, padx=2)
        
        buttons.grid(row=2, column=1, sticky=tk.SE)
        
    def save_schemas(self):
        print "Save schemas"
        if self._onsave:
            self._onsave(self._schemas, self._filename.get(), self._format.get())
        self.destroy()
        
    def get_filename(self):
        filename = tkFileDialog.askopenfilename()
        self._filename.set(filename)
        
class LoadSchemasDialog(tk.Toplevel):
    
    def __init__(self, master, **options):
        tk.Toplevel.__init__(self, master)
        
        self._schemas = []
        self._onload= options.get('onload') or None
        
        self.title('Load schemas')
        
        tk.Label(self, text='Load from: ').grid(row=0, column=0, sticky=tk.NW)
        
        self._filename = tk.StringVar()
        self._filename.set(schemas_file)
        tk.Entry(self, textvariable=self._filename).grid(row=0, column=1, sticky=tk.NW)
        tk.Button(self, text="Select file", command=self.get_filename).grid(row=0, column=3, sticky=tk.NW)       
        
        tk.Label(self, text='Format: ').grid(row=1, column=0, sticky=tk.NW)
        
        self._format = tk.StringVar()
        self._format.set('xml')
               
        tk.OptionMenu(self, self._format, 'xml', 'yaml').grid(row=1, column=1, sticky=tk.NW)
        
        buttons = tk.Frame(self)
        save = tk.Button(buttons, text="Load", command=self.load_schemas)
        save.pack(side=tk.LEFT, padx=2)
        
        cancel = tk.Button(buttons, text="Cancel", command=self.destroy)
        cancel.pack(side=tk.LEFT, padx=2)
        
        buttons.grid(row=2, column=1, sticky=tk.SE)
        
    def load_schemas(self):
        print "Load schemas"
        unserializer = conf.ConfigurationSchemasXMLUnserializer()
        schemas = unserializer.read(self._filename.get())
        
        if self._onload:
            self._onload(schemas)
            
        self.destroy()
        
    def get_filename(self):
        filename = tkFileDialog.askopenfilename()
        self._filename.set(filename)                  
            
class ConfigurationSchemaEditor(tk.Frame):
    def __init__(self, master, schema, **options):
        tk.Frame.__init__(self, master)
        
        self.schema = schema
        self._onsave = options.get('onsave') or None
        self._onremove = options.get('onremove') or None
        
        # ui
        props = tk.LabelFrame(self,text=schema.name + " configuration schema", padx=10, pady=10,font=('Arial',10, 'bold'))
        tk.Label(props, text="Name: ").grid(row=0, column=0, sticky=tk.W)
        self.schema_name = tk.StringVar()
        self.schema_name.set(schema.name or "")
        tk.Entry(props, textvariable=self.schema_name).grid(row=0, column=1, sticky=tk.W + tk.N)
        
        tk.Label(props, text="Parents:").grid(row=1, column=0, sticky=tk.W + tk.N)
        source_list = list(conf.ConfigurationSchema.configuration_schemas())
        source_list.remove(schema)
        self.parents = w.DoubleListSelector(props, source=source_list, selected=schema.parents())
        set_status_message(self.parents, "Add and remove parents to the configuration schema")
        self.parents.grid(row=1, column=1, sticky=tk.W)
        
        tk.Label(props, text="Documentation:").grid(row=2, column=0, sticky=tk.W + tk.N)
        self.schema_doc = tk.Text(props, width=60, height=10)
        self.schema_doc.insert(tk.END, schema.documentation)
        self.schema_doc.grid(row=2, column=1, sticky=tk.W)
        
        props.pack(expand=True, fill=tk.BOTH)
        
        buttons = tk.Frame(self)
        save = tk.Button(buttons, text="Save", command=self.save_schema)
        set_status_message(save, "Save changes to configuration schema")
        save.pack(side=tk.LEFT, padx=2)
        
        restore = tk.Button(buttons, text="Restore", command=self.restore_schema)
        set_status_message(restore, "Restore original configuration schema data")
        restore.pack(side=tk.LEFT, padx=2)
        
        remove = tk.Button(buttons, text="Remove", command=self.remove_schema)
        set_status_message(remove, "Remove the configuration schema")
        remove.pack(side=tk.LEFT, padx=2)
        
        buttons.pack(side=tk.RIGHT)
    
    def save_schema(self):
        # Validation
        errors = ''
        if self.schema_name.get() == '':
            errors = errors + 'Fill in the configuration schema name\n'
            
        if len(errors) > 0:
            tkMessageBox.showerror('Error', errors)
        else:
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
        self.parents = w.DoubleListSelector(props, source=conf.ConfigurationSchema.configuration_schemas(),
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
        # Validation
        errors = ''
        
        if self.schema_name.get() == '':
            errors = errors + 'Fill in the configuration schema name'
            
        if len(errors) > 0:
            tkMessageBox.showerror('Error', errors)
        else:
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
        f = tk.LabelFrame(self,text=section.name + ' section',padx=10, pady=10, font=('Arial',10, 'bold'))
        
        tk.Label(f, text="Name: ").grid(row=0, column=0, sticky=tk.W)
        self.section_name = tk.StringVar()
        self.section_name.set(section.name or "")
        tk.Entry(f, textvariable=self.section_name).grid(row=0, column=1, sticky=tk.W)
        
        tk.Label(f, text="Documentation").grid(row=1, column=0, sticky=tk.W)
        self.section_documentation = tk.Text(f, height=10, width=60)
        self.section_documentation.insert(tk.END, self.section.documentation)
        self.section_documentation.grid(row=1, column=1, sticky=tk.W)
        
        f.pack(fill=tk.BOTH, expand=True)
        
        buttons = tk.Frame(self)
        
        save = tk.Button(buttons, text="Save", command=self.save_section)
        save.pack(side=tk.LEFT, padx=2)
        set_status_message(save, "Update the section with the new data")
        
        restore = tk.Button(buttons, text="Restore", command=self.restore_section)
        restore.pack(side=tk.LEFT, padx=2)
        set_status_message(restore, "Restore the section data to its original form")
        
        remove = tk.Button(buttons, text="Remove", command=self.remove_section)
        remove.pack(side=tk.LEFT, padx=2)
        set_status_message(remove, "Remove the section")
        
        buttons.pack(side=tk.RIGHT)
        
    def save_section(self):
        # Validation
        errors = ''
        
        if self.section_name.get() == '':
            errors = errors + 'Fill the section name\n'
            
        if len(errors) > 0:
            tkMessageBox.showerror('Error', errors)
        else:
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
        # Validation
        errors = ''
        
        if self.section_name.get() == '':
            errors = errors + 'Fill the section name\n'
            
        if len(errors) > 0:
            tkMessageBox.showerror('Error', errors)
        else:
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
        options = tk.OptionMenu(self.f, self.option_type, *option_types, command=self.edit_option_type)
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
        # Validation
        errors = []
        if self.option_name.get() == '':
            errors.append('Enter the option name')
        
        if self.option_type.get() == '':
            errors.append('Select the option type')
            
        if len(errors) > 0:
            message = ''
            for error in errors:
                message = message + error + '\n'
                
            tkMessageBox.showerror('Error', message)
        else:
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
        
        self.f = tk.LabelFrame(self,text=option.name + " option", padx=10, pady=10, font=('Arial',10, 'bold'))
                
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
            print "Editor for : " + str(option.option_type.__class__) + str(editor)
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
            self._default_value_editor.set_value(option.default_value)
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
        
        self.f.pack(fill=tk.BOTH, expand=True)
        
        # Dependencies
        dependencies = tk.LabelFrame(self, text='Dependencies')
        
        self._dependency_editor = DependencyExpressionEditor(dependencies, option=self.option)
        self._dependency_editor.pack()
        dependencies.pack(fill=tk.X, expand=True)
                
        buttons = tk.Frame(self)
    
        save = tk.Button(buttons, text="Save", command=self.save_option)
        set_status_message(save, "Save option changes")
        save.pack(side=tk.LEFT, padx=2)
        
        restore = tk.Button(buttons, text="Restore", command=self.restore_option)
        set_status_message(restore, "Restore option original data")
        restore.pack(side=tk.LEFT, padx=2)
        
        remove = tk.Button(buttons, text="Remove", command=self.remove_option)
        set_status_message(remove, "Remove the option")
        remove.pack(side=tk.LEFT, padx=2)
        
        buttons.pack(side=tk.RIGHT)
        
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
            
            # Hack. Clean this!!
            if OptionTypeEditor.for_option_type(option_type) is not None:
                option_type = self.option_type_editor.option_type_instance()
                editor = OptionEditor.for_option_type(option_type.__class__)
                self._default_value_editor = editor(self.f, option_type=option_type)
            else:      
                editor = OptionEditor.for_option_type(option_type)
                self._default_value_editor = editor(self.f, option_type=option_type)
                
            #self._default_value_editor.set_value(self.option.default_value)
                
        else:
            self._default_value_editor = tk.Frame(self.f)
            
        self._default_value_editor.grid(row=4, column=1, sticky=tk.W) 
    
    def save_option(self):
        # Validation
        errors = ''
        if self.option_type.get() == '':
            errors = errors + 'Select option type' + '\n'
        if self.option_name.get() == '':
            errors = errors + 'Fill the option name' + '\n'
        
        if errors <> '':
            tkMessageBox.showerror('Error', errors)
        else:
            self.option.name = self.option_name.get()
            self.option.documentation = self.option_documentation.get(1.0, tk.END)
            if isinstance(self.option_type_editor, OptionTypeEditor):
                self.option.option_type = self.option_type_editor.option_type_instance()
            else:
                option_type = conf.OptionType.get_named(self.option_type.get())
                self.option.option_type = option_type()
                 
            self.option.is_required = self.option_required.get() == 1
            if self._default_value_var.get() == 1:
                self.option.default_value = self._default_value_editor.value()
            else:
                self.option.default_value = None
                
            expression, ast = self._dependency_editor.value()
            self.option.dependency_expression = ast
                
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
    
    def option_type_instance(self):
        return self.__class__.option_type()
         
class ChoiceOptionTypeEditor(OptionTypeEditor, w.ListEditor):
    option_type = conf.ChoiceOptionType
    
    def __init__(self, parent, option_type):
        OptionTypeEditor.__init__(self, parent, option_type)
        
        print "Option type: " + str(option_type)
        print "Options: " + str(option_type.options())
        
        self.options_var = tk.StringVar()
        self.options_var.set(' '.join(option_type.options()))
        
        w.ListEditor.__init__(self, parent, listvar=self.options_var)
        set_status_message(self, "The possible option choices")
        
    def options(self):
        return list(eval(self.options_var.get()))
    
    def option_type_instance(self):
        # Return an instance of the edited option type
        return conf.ChoiceOptionType(self.options())   
    
class ManyOptionTypeEditor(OptionTypeEditor):
    option_type = conf.ManyOptionType
    
    def __init__(self, parent, option_type):
        OptionTypeEditor.__init__(self, parent, option_type)
        
        #ui
        
        # Choose the option type
        option_types = map(lambda o: o.option_name(), conf.OptionType.__subclasses__())
        
        # Maybe we can remove this??
        option_types.remove('Many')
        option_types.remove('One of')
        option_types.remove('Maybe')
        
        self._option_type_var = tk.StringVar()
        
        if option_type.option_type is not None:
            value = self._option_type_var.set(option_type.option_type.name)
        else:
            value = option_types[0]
        
        
        self._option_type_selector = tk.OptionMenu(self, 
                                                   self._option_type_var, 
                                                   option_types[0], 
                                                   *option_types, 
                                                   command=self.select_option_type)
        self._option_type_selector.pack()
        
        if option_type.option_type is not None:
            editor = OptionTypeEditor.for_option_type(option_type)
                    
            if editor:
                self._option_type_editor = editor(self, option_type.option_type)
            else:
                self._option_type_editor = tk.Frame()
        else:
            self._option_type_editor = tk.Frame()
            
        self._option_type_editor.pack()
        
    def select_option_type(self, ev):
        option_type = conf.OptionType.get_named(self._option_type_var.get())
        
        editor = OptionTypeEditor.for_option_type(option_type)
                    
        self._option_type_editor.forget()     
        
        if editor:
            self._option_type_editor = editor(self, option_type())
        
        self._option_type_editor.pack()
        
    def option_type_instance(self):
        # Return an instance of the edited option type
               
        if isinstance(self._option_type_editor, OptionTypeEditor):
            option_type = self._option_type_editor.option_type_instance()
        else:
            option_type_class = conf.OptionType.get_named(self._option_type_var.get())
            option_type = option_type_class()
        
        return conf.ManyOptionType(option_type)       
        
class ConfigurationNavigator(tk.Frame):
    def __init__(self, master, configs):
        tk.Frame.__init__(self, master)
        
        # Configuration
        self._items = {}
        self._configs = configs
        self._option_editors = {}
        self._set_options = {}
        self._unset_options = {}
                
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
        ysb.pack(side=tk.RIGHT, fill=tk.Y)
        
        xsb = ttk.Scrollbar(self._left_panel, orient='horizontal', command=self._sections.xview)
        xsb.pack(side=tk.BOTTOM, fill=tk.X)
        
        self._sections.configure(yscroll=ysb.set, xscroll=xsb.set)
        
        if len(configs) > 0:
            self._config = configs[0]
            sections = self._config.sections()
            
            if len(sections) > 1:
                self._section = sections[0]
        
                for section in sections:
                    self.insert_section(section)
            
                self._sections.selection_set(self._sections.get_children()[0])
            
        self._sections.pack(fill=tk.Y, expand=True)
        self._left_panel.pack(side=tk.LEFT, fill=tk.Y)
           
        self._right_panel = tk.Frame(self, pady=10, relief=tk.FLAT)
        
        if len(configs) > 0 and len(sections) > 1:
            self.insert_section_editor(sections[0])
                           
        self._right_panel.pack(side=tk.LEFT, fill=tk.BOTH)
                        
        self.pack()
        
    def insert_section_editor(self, section, errors=None):
        #section_editor = ConfigurationSectionEditor(self._right_panel, 
        #                                            config=self._config, 
        #                                            section=section,
        #                                            errors=errors,
        #                                            onsave=lambda section: self.save_section(section))
        section_editor = ConfigurationSectionViewer(self._right_panel,
                                                    config=self._config,
                                                    section=section,
                                                    errors=errors)
        section_editor.pack(expand=True, fill=tk.BOTH)        
        
    def select_config(self, ev=None):
        # Grab the selected configuration
        selection = self._configs_list.curselection()
        self._config = self._configs[int(selection[0])]
        
        # Refill the sections list with the configuration sections
        self._sections.delete(*self._sections.get_children())
        self._items = {}
        
        sections = self._config.sections()
        
        if len(sections) > 1:
            self._section = sections[0]
            for section in sections:
                self.insert_section(section)
            
            self._sections.selection_set(self._sections.get_children()[0])
        
        # Clear the right panel
        self._right_panel.forget()
        self._right_panel = tk.Frame(self, pady=10, relief=tk.FLAT)
                
        if len(sections) > 1:
            # Put the options editing on the right panel
            self.insert_section_editor(sections[0])
                
        self._right_panel.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        
    def select_section(self, ev):
        id = self._sections.identify_row(ev.y)
        self._section = self._items[id]
        print "Section: " + self._section.name
        
        # Clear the right panel
        self._right_panel.forget()
        self._right_panel = tk.Frame(self, pady=10, relief=tk.FLAT)
        
        # Put the options editing on the right panel
        self.insert_section_editor(self._section)
        
        self._right_panel.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)               
        
    def insert_section(self, section, parent=''):
        sid = self._sections.insert(parent, 'end', text=section.name)
        self._items[sid] = section
        
        for subsection in section.subsections():
            self.insert_section(subsection, sid)
            
    
    def restore_section(self, section):
        print "Restore section " + section.name
        
    def sections_popup(self, ev):
        print "Sections popup"
        item = self._sections.identify_row(ev.y)
        section = self._items[item]
        print "Section: " +  section.name
                 
    def configs_popup(self, ev):
        print "Configs popup"
        
        def create_configs_menu():
            popup.add_command(label="Add configuration", command=self.create_config)
            popup.add_command(label="Save configurations", command=self.save_configs)
            popup.add_command(label="Load configurations", command=self.load_configs)
            popup.add_command(label="Validate configurations", command=self.validate_configs)
            
        index = self._configs_list.nearest(ev.y)
        
        # create a menu
        popup = tk.Menu(self, tearoff=0)
        
        if index >= 0:
            _, yoffset, _, height = self._configs_list.bbox(index)
        
            if ev.y > height + yoffset + 5: # XXX 5 is a niceness factor :)
                # Outside of widget.
                create_configs_menu()
            else:
                config_name = self._configs_list.get(index)
                config = next((config for config in self._configs if config.name == config_name), None)
                print "Config: " + str(config)
                    
                popup.add_command(label="Remove", command=lambda:self.remove_config(config, index))
                popup.add_command(label="Edit", command=lambda:self.edit_config(config, index))
                popup.add_command(label="Validate", command=lambda:self.validate_config(config, index))
        else:
            create_configs_menu()            
            
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
        def save_config(config):
            self._configs.append(config)
            self._configs_list.insert(tk.END, config.name)
                        
        ConfigurationEditor(self, conf.Configuration(), 
                                  self._configs, title='New configuration',
                                  onsave=save_config)
    def load_configs(self):
        def load_configs(configs):
            print "Load configs"
            
            self._configs_list.delete(0, tk.END)
            self._configs = []
            
            for config in configs:
                self._configs.append(config)
                self._configs_list.insert(tk.END, config.name)
                
            self._configs_list.selection_set(0)
            self.select_config()
                
        dialog = LoadConfigurationsDialog(self, onload=load_configs)
        self.wait_window(dialog)
        
    def save_configs(self):
        def save_configs(configs, filename, format):
            serializer = conf.ConfigurationsXMLSerializer()
            for config in configs:
                serializer.serialize(config)
            serializer.write(filename)
            tkMessageBox.showinfo('Configurations saved successfully', 'Configurations have been saved on ' + filename)
        
        error_msg = ''    
        for config in self._configs:
            errors = config.validate()
            if errors:
                error_msg = error_msg + '\n' + config.name + " configuration is invalid: \n"
                for error in errors:
                    error_msg = error_msg + "    " + error['message'] + '\n'
        def open_save_dialog():
            dialog = SaveConfigurationsDialog(self, self._configs, onsave=save_configs)
            self.wait_window(dialog)
        if error_msg <> '':
            answer = tkMessageBox.askquestion('Save configurations?', 'There are invalid configurations, save anyway? \n' + error_msg)
            if answer == 'yes':
                open_save_dialog()
        else:
            open_save_dialog()
        
    def edit_config(self, config, index):
        print "Edit config " + str(config)
        editor = ConfigurationEditor(self, config, self._configs)
        self.wait_window(editor)        
        
    def remove_config(self, config, index):
        print "Remove config " + str(config)
        answer = tkMessageBox.askquestion('Remove?', 'Remove ' + config.name + ' configuration?')
        if answer == 'yes':
            self._configs.remove(config)
            self._configs_list.delete(index)
            
    def validate_config(self, config, index):
        errors = config.validate()
        
        if errors:
            errors_msg = ''
            for error in errors:
                errors_msg = errors_msg + error['message'] + "\n"
            tkMessageBox.showerror('Invalid configuration', 'The configuration is invalid \n\n' + errors_msg)
        else:
            tkMessageBox.showinfo('Valid configuration', 'The configuration is valid')
    
    def validate_configs(self):
        error_msg = ""
        for config in self._configs:
            errors = config.validate()
            if errors:
                error_msg = error_msg + '\n' + config.name + " configuration is invalid: \n"
                for error in errors:
                    error_msg = error_msg + "    " + error['message'] + '\n'
        if error_msg <> '':
            tkMessageBox.showerror('Invalid configurations', 'There are invalid configurations\n' + error_msg)
        else:
            tkMessageBox.showinfo('Valid configurations', 'All configurations are valid')
            
    def save_section(self, section):
        print "Save section"
        
class ConfigurationSectionEditor(tk.Frame):
    def __init__(self, parent, config, section, errors=[], **options):
        
        tk.Frame.__init__(self, parent)
        
        self._onsave = options.get('onsave')
        self._config = config
        self._section = section
        
        self._unset_options = {}
        self._set_options = {}
        
        self.redraw()
    
    def redraw(self, errors=[]):
        for child in self.winfo_children():
            child.forget()
            
        tk.Label(self, text=self._config.name + ' configuration', font=('Verdana', 10, 'bold')).pack()
        if self._config.documentation is not None and self._config.documentation <> '':
            tk.Label(self, text=self._config.documentation, font=('Verdana', 8, 'italic')).pack()
        
        config_props = tk.Frame(self)
        tk.Label(config_props, text='Schema: ' + self._config.schema.name, font=('Verdana', 8,'normal')).pack(side=tk.LEFT, padx=5)
        
        if self._config.parent is not None:
            tk.Label(config_props, text='Parent: ' + self._config.parent.name, font=('Verdana', 8,'normal')).pack(side=tk.LEFT, padx=5)
        config_props.pack()        
        
        self._option_editors = {}
        
        self._errors_panel = None
        
        if errors:
            print "Errors!!!"
            self._errors_panel = tk.LabelFrame(self, text='Errors', font=('Arial',10, 'bold'), padx=10, pady=10)
            for error in errors.values():
                tk.Label(self._errors_panel, text=error['message'], foreground='Red').pack(pady=5, fill=tk.X)
            self._errors_panel.pack(fill=tk.X, expand=True)
        
        options = tk.LabelFrame(self, text=self._section.name, font=('Arial',10, 'bold'), padx=10, pady=10)
        row = 0
        if self._section.documentation is not None and self._section.documentation <> '':
            tk.Label(options, text=self._section.documentation, font=('Verdana', 8, 'italic')).grid(row=row, column=0, columnspan=3)
            row = row + 1       
        
        for option in self._section.options():
            option_value, origin = self._config.option_value(option)
            
            label_text = option.name
            if option.is_required and not option.default_value:
                label_text = label_text + ' (required)'
                
            label_text=label_text + ':'
            
            label_color = 'Black'
            if errors and errors.get(option.name):
                label_color = 'Red'
                
            label = tk.Label(options, text=label_text, foreground=label_color)
            
            # Option label popup
            label.bind('<ButtonRelease-3>', lambda ev, option=option: self.option_popup(ev, option))
            
            label.grid(row=row, column=0, padx=30, pady=10, sticky=tk.NW)
            
            #print "Option" + str(option.option_type)
            option_editor_class = OptionEditor.for_option_type(option.option_type.__class__)
                        
            #print "Editor" + str(option_editor)
                         
            option_editor = option_editor_class(options, option_schema=option)
            
            option_editor.grid(row=row, column=1, padx=10, pady=10, sticky=tk.NW)
            
            if option_value:
                option_editor.set_value(option_value)
            
            self._option_editors[option] = option_editor
            
            documentation = option.documentation
            if origin and origin <> self._config:
                documentation = documentation + '\n\n This option is set in ' + origin.name + ' configuration.'
                
            if not option_value and option.default_value:
                documentation = documentation + '\n\n This option is set to its default value'
                
            doc = tk.Label(options, text=documentation, font=('Verdana', 8, 'italic'))
            doc.grid(row=row, column=2, padx=20, pady=10, sticky=tk.NW)
                
            row = row + 1
            
        options.pack(fill=tk.BOTH, expand=tk.Y)
            
        buttons = tk.Frame(self)
        
        restore = tk.Button(buttons, text="Restore", command=lambda:self.restore_section(section))
        restore.pack(side=tk.RIGHT, padx=2)
        
        save = tk.Button(buttons, text="Save", command=lambda: self.save_section(self._section))
        save.pack(side=tk.RIGHT, padx=2)
        
        buttons.pack(fill=tk.X)
    
    def set_option(self, option):
        print "Set option " + str(option)
        self._set_options[option.name] = option
        if self._unset_options.get(option.name):
            del self._unset_options[option.name]
        self._option_editors[option].disable()
            
    def unset_option(self, option):
        print "Unset option " + str(option.name)
        self._unset_options[option.name] = option
        if self._set_options.get(option.name):
            del self._set_options[option.name]
        self._option_editors[option].disable()
            
    def restore_option(self, option):
        print "Restore option " + str(option)
        
    def option_popup(self, ev, option):
        print "Option " + option.name + " popup"
        # create a menu
        popup = tk.Menu(self, tearoff=0)
        
        popup.add_command(label="Set", command=lambda:self.set_option(option))
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
            
    def save_section(self, section):
        print 'Saving section ' + section.name
        for option, editor in self._option_editors.iteritems():
            if editor.value_changed():
                print "Setting option " + str(option.path()) + " value to " + str(editor.value())
                self._config.set_option_value(option, editor.value())
                
        for option in self._unset_options.values():
            self._config.unset_option(option)
            
        for option in self._set_options.values():
            editor = self._option_editors.get(option)
            self._config.set_option_value(option, editor.value())
        
        self._unset_options={}
        self._set_options={}
                
        errors = section.validate(self._config)
        if errors:
            self.redraw(errors)
            configurator.status.set('There are errors')
        else:
            configurator.status.set('Section saved')
                
            # We do this to clear possible past errors
            self.redraw()
            
        if self._onsave is not None:
            self._onsave(section)
            
class ConfigurationSectionViewer(tk.Frame):
    def __init__(self, parent, config, section, errors=[], **options):
        
        tk.Frame.__init__(self, parent)
        
        self._onsave = options.get('onsave')
        self._config = config
        self._section = section
              
        self.redraw()
    
    def redraw(self, errors=[]):
        for child in self.winfo_children():
            child.forget()
            
        tk.Label(self, text=self._config.name + ' configuration', font=('Verdana', 10, 'bold')).pack()
        if self._config.documentation is not None and self._config.documentation <> '':
            tk.Label(self, text=self._config.documentation, font=('Verdana', 8, 'italic')).pack()
        
        config_props = tk.Frame(self)
        tk.Label(config_props, text='Schema: ' + self._config.schema.name, font=('Verdana', 8,'normal')).pack(side=tk.LEFT, padx=5)
        
        if self._config.parent is not None:
            tk.Label(config_props, text='Parent: ' + self._config.parent.name, font=('Verdana', 8,'normal')).pack(side=tk.LEFT, padx=5)
        config_props.pack()        
        
        self._errors_panel = None
        
        if errors:
            print "Errors!!!"
            self._errors_panel = tk.LabelFrame(self, text='Errors', font=('Arial',10, 'bold'), padx=10, pady=10)
            for error in errors.values():
                tk.Label(self._errors_panel, text=error['message'], foreground='Red').pack(pady=5, fill=tk.X)
            self._errors_panel.pack(fill=tk.X, expand=True)
        
        options = tk.LabelFrame(self, text=self._section.name, font=('Arial',10, 'bold'), padx=10, pady=10)
        row = 0
        if self._section.documentation is not None and self._section.documentation <> '':
            tk.Label(options, text=self._section.documentation, font=('Verdana', 8, 'italic')).grid(row=row, column=0, columnspan=3)
            row = row + 1       
        
        for option in self._section.options():
            if self._config.option_is_enabled(option):
                option_value, origin = self._config.option_value(option)
                
                label_text = option.name
                if option.is_required and not option.default_value:
                    label_text = label_text + ' (required)'
                    
                label_text=label_text + ':'
                
                label_color = 'Black'
                if errors and errors.get(option.name):
                    label_color = 'Red'
                    
                label = tk.Label(options, text=label_text, foreground=label_color)
                
                # Option label popup
                label.bind('<ButtonRelease-3>', lambda ev, option=option: self.option_popup(ev, option))
                
                label.grid(row=row, column=0, padx=30, pady=10, sticky=tk.NW)
                
                # Option value
                value_display = tk.Label(options, text=str(option_value))
                value_display.bind('<Double-Button-1>', lambda ev, option=option: self.edit_option(ev, option))
                set_status_message(value_display, 'Double click to edit')
                
                value_display.grid(row=row, column=1, padx=10, pady=10, sticky=tk.NW)
                
                documentation = option.documentation
                if origin and origin <> self._config:
                    documentation = documentation + '\n\n This option is set in ' + origin.name + ' configuration.'
                    
                if not option_value and option.default_value:
                    documentation = documentation + '\n\n This option is set to its default value'
                    
                doc = tk.Label(options, text=documentation, font=('Verdana', 8, 'italic'))
                doc.grid(row=row, column=2, padx=20, pady=10, sticky=tk.NW)
                    
                row = row + 1
            
        options.pack(fill=tk.BOTH, expand=tk.Y)       
        
    def edit_option(self, ev, option):
        self.wait_window(OptionEditorDialog(self, option, self._config, onsave=self.save_option))
  
    def set_option(self, option):
        print "Set option " + str(option)
        editor = self._option_editors.get(option)
        self._config.set_option_value(option, editor.value())
        self.redraw()
            
    def unset_option(self, option):
        print "Unset option " + str(option.name)
        self._config.unset_option(option)
        self.redraw()
        
    def save_option(self, option, editor):
        if editor.value_changed():
            print "Setting option " + str(option.path()) + " value to " + str(editor.value())
            self._config.set_option_value(option, editor.value())
            self.redraw()
            
    def restore_option(self, option):
        print "Restore option " + str(option)
        
    def option_popup(self, ev, option):
        print "Option " + option.name + " popup"
        # create a menu
        popup = tk.Menu(self, tearoff=0)
        
        popup.add_command(label="Set", command=lambda:self.set_option(option))
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
            
class OptionEditorDialog(tk.Toplevel):
    def __init__(self, master, option, config, **options):
        tk.Toplevel.__init__(self, master)
        
        self._config = config
        self._option = option
        self._onsave = options.get('onsave')
        
        option_editor_class = OptionEditor.for_option_type(option.option_type.__class__)
                        
        self._option_editor = option_editor_class(self, option_schema=option)
        
        option_value, origin = self._config.option_value(option)
            
        if option_value:
            self._option_editor.set_value(option_value)
        
        self._option_editor.pack()
        
        buttons = tk.Frame(self)
        save = tk.Button(buttons, text='Save', command=self.save_option)
        save.pack(side=tk.LEFT)
        
        cancel = tk.Button(buttons, text='Cancel', command=self.destroy)
        cancel.pack(side=tk.LEFT)
        buttons.pack()
    
    def save_option(self):
        value = self._option_editor.value()
        self._config.set_option_value(self._option, value)
        self.destroy()
        if self._onsave is not None:
            self._onsave(self._option, self._option_editor)          
        
class ConfigurationEditor(tk.Toplevel):
    
    def __init__(self, parent, config, configs, **options):
        tk.Toplevel.__init__(self, parent)
        
        self._config = config
        self._configs = configs
        self._onsave = options.get('onsave') or None
        
        self.transient(parent)
        title = options.get('title') or config.name + ' configuration' 
        self.title(title)
        
        self.geometry("+%d+%d" % (parent.winfo_rootx()+50,
                                  parent.winfo_rooty()+50))
        
        # Name editing
        tk.Label(self, text='Name: ').grid(row=0, column=0, sticky=tk.NW)
        self._config_name = tk.StringVar()
        self._config_name.set(config.name)
        tk.Entry(self, textvariable=self._config_name).grid(row=0, column=1, sticky=tk.NW)
        
        # Documentation editing
        tk.Label(self, text="Documentation:").grid(row=1, column=0, sticky=tk.W + tk.N)
        self._config_doc = tk.Text(self, width=60, height=10)
        self._config_doc.insert(tk.END, config.documentation)
        self._config_doc.grid(row=1, column=1, sticky=tk.W)
        
        tk.Label(self, text="Schema: ").grid(row=2, column=0, sticky=tk.W + tk.N)
        self._schemas = tk.Listbox(self)
        
        index = 0
        for schema in self.all_schemas():
            self._schemas.insert(index, schema.name)
            if schema == config.schema:
                self._schemas.selection_set(index)
            index = index + 1
        self._schemas.grid(row=2, column=1, sticky=tk.NW)
        
        # Parent
        tk.Label(self, text="Parent: ").grid(row=3, column=0, sticky=tk.NW)
        self._config_parent = tk.StringVar()
        if config.parent:
            self._config_parent.set(config.parent.name)
            
        self._config_parents = tk.OptionMenu(self, self._config_parent, *[''] + map(lambda c: c.name, configs))
        self._config_parents.grid(row=3, column=1, sticky=tk.NW)
        
        # Dialog buttons
        buttons = tk.Frame(self)
        save = tk.Button(buttons, text="Save", command=self.save_config)
        set_status_message(save, "Save changes to configuration")
        save.pack(side=tk.LEFT, padx=2)
        
        cancel = tk.Button(buttons, text="Cancel", command=self.destroy)
        set_status_message(cancel, "Cancel the editing")
        cancel.pack(side=tk.LEFT, padx=2)
              
        buttons.grid(row=4, column=1, sticky=tk.SE)
    
    def all_schemas(self):
        return conf.ConfigurationSchema.configuration_schemas()
    
    def save_config(self):
        print "Save config"
        
        # Validation
        errors = '' 
        
        if self._config_name.get() == '':
            errors = errors + 'Fill in the configuration name\n'
            
        if len(self._schemas.curselection()) == 0:
            errors = errors + 'Select the configuration schema\n'
            
        if len(errors) > 0:
            tkMessageBox.showerror('Error', errors)
        else:
            self._config.name = self._config_name.get()
            self._config.documentation = self._config_doc.get(1.0, tk.END)
            
            index = self._schemas.curselection()
            schema = self.all_schemas()[int(index[0])]
            self._config.schema = schema
            
            if self._config_parent.get() <> '':
                parent = next((config for config in self._configs if config.name == self._config_parent.get()), None)
            else:
                parent = None
            
            self._config.parent = parent
            
            if self._onsave:
                self._onsave(self._config)
                
            self.destroy()
        
class LoadConfigurationsDialog(tk.Toplevel):
    
    def __init__(self, master, **options):
        tk.Toplevel.__init__(self, master)
        
        self._configs = []
        self._onload= options.get('onload') or None
        
        self.title('Load configs')
        
        tk.Label(self, text='Load from: ').grid(row=0, column=0, sticky=tk.NW)
        
        self._filename = tk.StringVar()
        self._filename.set(configs_file)
        tk.Entry(self, textvariable=self._filename).grid(row=0, column=1, sticky=tk.NW)
        tk.Button(self, text="Select file", command=self.get_filename).grid(row=0, column=3, sticky=tk.NW)       
        
        tk.Label(self, text='Format: ').grid(row=1, column=0, sticky=tk.NW)
        
        self._format = tk.StringVar()
        self._format.set('xml')
               
        tk.OptionMenu(self, self._format, 'xml', 'yaml').grid(row=1, column=1, sticky=tk.NW)
        
        buttons = tk.Frame(self)
        save = tk.Button(buttons, text="Load", command=self.load_configs)
        save.pack(side=tk.LEFT, padx=2)
        
        cancel = tk.Button(buttons, text="Cancel", command=self.destroy)
        cancel.pack(side=tk.LEFT, padx=2)
        
        buttons.grid(row=2, column=1, sticky=tk.SE)
        
    def load_configs(self):
        print "Load configs"
        unserializer = conf.ConfigurationsXMLUnserializer()
        configs = unserializer.read(self._filename.get())
        
        if self._onload:
            self._onload(configs)
            
        self.destroy()
        
    def get_filename(self):
        filename = tkFileDialog.askopenfilename()
        self._filename.set(filename)
        
class SaveConfigurationsDialog(tk.Toplevel):
    
    def __init__(self, master, configs, **options):
         
        tk.Toplevel.__init__(self, master)
        
        self._configs = configs
        self._onsave = options.get('onsave') or None
        
        self.title('Save configs')
        
        tk.Label(self, text='Save to: ').grid(row=0, column=0, sticky=tk.NW)
        
        self._filename = tk.StringVar()
        self._filename.set(configs_file)
        tk.Entry(self, textvariable=self._filename).grid(row=0, column=1, sticky=tk.NW)
        tk.Button(self, text="Select file", command=self.get_filename).grid(row=0, column=3, sticky=tk.NW)       
        
        tk.Label(self, text='Format: ').grid(row=1, column=0, sticky=tk.NW)
        
        self._format = tk.StringVar()
        self._format.set('xml')
               
        tk.OptionMenu(self, self._format, 'xml', 'yaml').grid(row=1, column=1, sticky=tk.NW)
        
        buttons = tk.Frame(self)
        save = tk.Button(buttons, text="Save", command=self.save_configs)
        save.pack(side=tk.LEFT, padx=2)
        
        cancel = tk.Button(buttons, text="Cancel", command=self.destroy)
        cancel.pack(side=tk.LEFT, padx=2)
        
        buttons.grid(row=2, column=1, sticky=tk.SE)
        
    def save_configs(self):
        print "Save configs"
        if self._onsave:
            self._onsave(self._configs, self._filename.get(), self._format.get())
        self.destroy()
        
    def get_filename(self):
        filename = tkFileDialog.askopenfilename()
        self._filename.set(filename)              
                
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
        self._option_type = args.get('option_type')
        if self._option_type is None and self._option_schema is not None:
            self._option_type = self._option_schema.option_type
            
        if args.get('option'):
            self._option_schema = args.get('option').schema
                
    @classmethod
    def for_option(cls, option):
        return cls.for_option_schema(option.schema)
    
    @classmethod
    def for_option_schema(cls, option_schema):
        return cls.for_option_type(option_schema.option_type.__class__)
       
    @classmethod
    def for_option_type(cls, option_type):
        subclasses = OptionEditor.__subclasses__()
        return next((editor for editor in subclasses if editor.option_type == option_type), None)
        
class StringOptionEditor(OptionEditor):
    option_type = conf.StringOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        self._initial_value = ''
        if self._option_schema and self._option_schema.default_value:
            self._initial_value = self._option_schema.default_value
             
        self._var = tk.StringVar()
        self._var.set(self._initial_value)
            
        self._entry = tk.Entry(self, textvariable=self._var)
        self._entry.pack()
        
    def disable(self):
        self._entry.configure(state=tk.DISABLED)
        
    def value(self):
        value = self._var.get() 
        if value == '':
            return None
        else:
            return value
    
    def set_value(self, value):
        self._var.set(value)
        self._initial_value = value
        
    def value_changed(self):
        return self.value() <> self._initial_value
        
class NumberOptionEditor(OptionEditor):
    option_type = conf.NumberOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        self._initial_value = None
        if self._option_schema and self._option_schema.default_value:
            self._initial_value = self._option_schema.default_value
            
        self._var = tk.StringVar()
        if self._initial_value:
            self._var.set(str(self._initial_value))
            
        vcmd = (self.register(self.validate),
                '%d', '%i', '%P', '%s', '%S', '%v', '%V', '%W')
        
        #self._sb = tk.Spinbox(self, textvariable=self._var)
        self._sb = tk.Entry(self, textvariable=self._var, validate='key', validatecommand=vcmd)
        self._sb.pack()
        
    def validate(self, action, index, value_if_allowed,
                       prior_value, text, validation_type, trigger_type, widget_name):
        if text in '0123456789.-+':
            try:
                float(value_if_allowed)
                return True
            except ValueError:
                return False
        else:
            return False
    
    def disable(self):
        self._sb.configure(state=tk.DISABLED)
        
    def value(self):
        value = self._var.get()
        if value <> '':
            return int(self._var.get())
        else:
            return None
    
    def set_value(self, value):
        self._var.set(value)
        
    def value_changed(self):
        return self.value() <> self._initial_value
        
class BooleanOptionEditor(OptionEditor):
    option_type = conf.BooleanOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        self._initial_value = None
        
        if self._option_schema and self._option_schema.default_value is not None:
            self._initial_value = self._option_schema.default_value        
        
        self._var = tk.IntVar()
        
        if self._initial_value is not None:
            self._var.set(1 if self._initial_value else 0)
                        
        self._cb = tk.Checkbutton(self, variable=self._var)
        self._cb.pack()
        
    def disable(self):
        self._cb.configure(state=tk.DISABLED)
        
    def value(self):
        return self._var.get() == 1
    
    def set_value(self, value):
        self._var.set(1 if value else 0)
    
    def value_changed(self):
        return self.value() <> self._initial_value
        
class ChoiceOptionEditor(OptionEditor):
    option_type = conf.ChoiceOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        self._initial_value = None
        
        if self._option_schema and self._option_schema.default_value is not None:
            self._initial_value = self._option_schema.default_value        
        
        self._var = tk.StringVar()
        
        if self._initial_value is not None:
            self._var.set(self._initial_value)
        
        self._lb = tk.OptionMenu(self, self._var, *self._option_type.options())
        self._lb.pack()
        
    def disable(self):
        self._lb.configure(state=tk.DISABLED)
        
    def value(self):
        return self._var.get()
        
    def set_value(self, value):
        self._var.set(value)
        self._initial_value = value
    
    def value_changed(self):
        return self._initial_value <> self.value() 
    
class TimezoneOptionEditor(OptionEditor):
    option_type = conf.TimezoneOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        self._lb = tk.Listbox(self)
        
        index = 0
        for tz in pytz.all_timezones:
            self._lb.insert(index, str(tz))
            if self._option_schema and self._option_schema.default_value == str(tz):
                self._lb.selection_set(index)
            index = index + 1           
            
        ysb = ttk.Scrollbar(self, orient='vertical', command=self._lb.yview)
        self._lb.configure(yscroll=ysb.set)
        self._lb.pack(side=tk.LEFT)
        ysb.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
    
    def value(self):
        selections = self._lb.curselection()
        if len(selections) < 1:
            return None
        else:
            selection = pytz.all_timezones[int(selections[0])]
            return selection
        
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
        
        self._initial_value = ''
        if self._option_schema and self._option_schema.default_value:
            self._initial_value = self._option_schema.default_value
             
        self._var = tk.StringVar()
        self._var.set(self._initial_value)
                       
        self._entry = tk.Entry(self, textvariable=self._var)
        self._entry.pack()
        
        self._select_btn = tk.Button(self, text='Select Color', command=self.getColor)
        self._select_btn.pack()
        
    def disable(self):
        self._entry.configure(state=tk.DISABLED)
        self._select_btn.configure(state=tk.DISABLED)
        
    def getColor(self):
        tuple, hex = tkColorChooser.askcolor()
        self._var.set(hex)
        
    def set_value(self, value):
        self._var.set(value)
        self._initial_value = value
        
    def value(self):
        return self._var.get()
    
    def value_changed(self):
        return self.value() <> self._initial_value
        
class FilenameOptionEditor(OptionEditor):
    option_type = conf.FilenameOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        self._initial_value = ''
        if self._option_schema and self._option_schema.default_value:
            self._initial_value = self._option_schema.default_value
             
        self._var = tk.StringVar()
        self._var.set(self._initial_value)
            
        tk.Entry(self, textvariable=self._var).pack()
        tk.Button(self, text='Select file', command=self.getFilename).pack()

    def getFilename(self):
        filename = tkFileDialog.askopenfilename()
        self._var.set(filename)
        
    def value(self):
        return self._var.get()
    
    def set_value(self, value):
        self._var.set(value)
        
    def value_changed(self):
        return self.value() <> self._initial_value
        
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
        
        if self._option_schema and self._option_schema.default_value:
            date = self._option_schema.default_value
        else:
            date = None
        
        self._calendar = tkCalendar.Calendar(self,date=date,dateformat="%d/%m/%Y")
        self._calendar.pack()
        
    def value(self):
        return self._calendar.dt
        
class DatetimeOptionEditor(OptionEditor):
    option_type = conf.DatetimeOptionType
    
    def __init__(self, master, **options):
        OptionEditor.__init__(self, master, **options)
        
        if self._option_schema and self._option_schema.default_value:
            self._date = self._option_schema.default_value
        else:
            self._date = None
            
        self._calendar = tkCalendar.Calendar(self,date=self._date,dateformat="%d/%m/%Y")
        self._calendar.pack()
        
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
        
    def value(self):
        return self._calendar.dt
    
    def set_value(self, value):
        self._calendar.dt = value
        self._calendar.showmonth()
    
    def value_changed(self):
        return self._calendar.dt <> self._date
    
class DependencyExpressionEditor(tk.Frame):
    def __init__(self, parent, option):
        tk.Frame.__init__(self, parent)
        
        self._schema = option.schema()
        
        # UI    
            
        self._expression_entry = tk.Text(self, width=60, height=10)
        if option.dependency_expression is not None:
            self._expression_entry.insert(tk.END, str(option.dependency_expression))
        self._expression_entry.pack()
        
        self._expression_editor = DependencyExpressionGraphicalEditor(self, option.dependency_expression, self._schema)
        self._expression_editor.pack()
    
    def value(self):
        expression = self._expression_entry.get(1.0, tk.END).strip()
        if expression <> '':
            print "Parsing expression: " + expression
            # Parse the expression
            try:
                ast = conf.DependencyExpressionParser.parse_expression(expression)
            except grako.FailedParse as e:
                tkMessageBox.showerror('Parse error', e.message)
                
            tkMessageBox.showinfo('Success', str(ast))
            
            return expression, ast
            
        else:
            return None, None
        
class DependencyExpressionGraphicalEditor(tk.Frame):
    def __init__(self, parent, expression=None, schema=None):
        tk.Frame.__init__(self, parent)
        
        self._expression = expression
        self._schema = schema
        
        self._path_selector = PathSelector(self, schema=schema)
        self._path_selector.pack(side=tk.LEFT, padx=3)
        
        self._comparator = tk.StringVar()
        self._comparator_selector = tk.OptionMenu(self, self._comparator, '=', '<>', '>','<')
        self._comparator_selector.pack(side=tk.LEFT, padx=3)
        
        self._value = tk.StringVar()
        
        # Wrong. Make this option type dependent. (ie. Checkbox for booleans, Combobox for choices, etc)
        self._value_editor = tk.Entry(self, textvariable=self._value)
        self._value_editor.pack(side=tk.LEFT, padx=3)
        
        self._logical_connector = tk.StringVar()
        self._logical_connector_selector = tk.OptionMenu(self, self._logical_connector, 'AND', 'OR', 'XOR')
        self._logical_connector_selector.pack(side=tk.LEFT, padx=3)
               
class PathSelector(tk.Frame):
    def __init__(self, parent, path=None, schema=None):
        tk.Frame.__init__(self, parent)
        
        self._schema = schema
                
        self._section_var = tk.StringVar()
        
        self._select_section = tk.OptionMenu(self, self._section_var, *map(lambda s: s.name, self._schema.sections()), command=self.select_section)
        self._select_section.pack(side=tk.LEFT, padx=3)
        
    def select_section(self, ev):
        if self._section_var.get() <> '':
            section = self._schema.get_section(self._section_var.get())
            
            #Redraw
            for child in self.winfo_children():
                child.forget()
            
            self._section_var = tk.StringVar()
            self._section_var.set(section.name)
        
            self._select_section = tk.OptionMenu(self, self._section_var, *map(lambda s: s.name, self._schema.sections()), command=self.select_section)
            self._select_section.pack(side=tk.LEFT, padx=3)
                        
            self._option_var = tk.StringVar()
            self._select_option = tk.OptionMenu(self, self._option_var, *map(lambda o: o.name, section.options()))           
            self._select_option.pack(side=tk.LEFT, padx=3)
            
class FullConfigurator(tk.Frame):
    def __init__(self, parent, configs=[], schemas=None):
        
        if schemas is None:
            schemas = conf.ConfigurationSchema.configuration_schemas()
                
        tk.Frame.__init__(self, parent, relief=tk.SUNKEN)
        
        parent.title('Configurator')
        
        # Menubar
        self.menu_bar = tk.Menu(self)
        
        # Configs menu
        configs_menu = tk.Menu(self.menu_bar)
        configs_menu.add_command(label="New", command=self.create_config)
        configs_menu.add_command(label="Save", command=self.save_configs)
        configs_menu.add_command(label="Load", command=self.load_configs)
        self.menu_bar.add_cascade(label='Configurations', menu=configs_menu)
        
        # Schemas menu
        schemas_menu = tk.Menu(self.menu_bar)
        schemas_menu.add_command(label="New", command=self.create_schema)
        schemas_menu.add_command(label="Save", command=self.save_schemas)
        schemas_menu.add_command(label="Load", command=self.load_schemas)
        self.menu_bar.add_cascade(label='Schemas', menu=schemas_menu)
        
        # Help menu
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
        
        self._schemas_nav = ConfigurationSchemaNavigator(self, schemas)
        self._configs_nav = ConfigurationNavigator(self, configs) 
        
        tabs.add(self._configs_nav, text='Configurations')
        tabs.add(self._schemas_nav, text='Configuration schemas')
                             
        tabs.pack(fill=tk.BOTH, expand=True, padx=2, pady=3)
        
        # Status bar
        self.status = w.StatusBar(self)
        self.status.pack(side=tk.BOTTOM, fill=tk.X)
        
    def create_config(self):
        self._configs_nav.create_config()
        
    def save_configs(self):
        self._configs_nav.save_configs()
        
    def load_configs(self):
        self._configs_nav.load_configs()
        
    def create_schema(self):
        self._schemas_nav.create_schema()
        
    def save_schemas(self):
        self._schemas_nav.save_schemas()
        
    def load_schemas(self):
        self._schemas_nav.load_schemas()
        
    def help_about(self):
        d = AboutDialog(self)

        self.wait_window(d)
        
    def quit(self):
        root.quit()
        
class Configurator(tk.Frame):
    def __init__(self, parent, configs=[]):
        
        tk.Frame.__init__(self, parent, relief=tk.SUNKEN)
        
        parent.title('Configurator')
        
        # Menubar
        self.menu_bar = tk.Menu(self)
        
        # Configs menu
        configs_menu = tk.Menu(self.menu_bar)
        configs_menu.add_command(label="New", command=self.create_config)
        configs_menu.add_command(label="Save", command=self.save_configs)
        configs_menu.add_command(label="Load", command=self.load_configs)
        configs_menu.add_command(label="Validate", command=self.validate_configs)
        self.menu_bar.add_cascade(label='Configurations', menu=configs_menu)
        
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
            
        self._configs_nav = ConfigurationNavigator(self, configs)    
        
        self._configs_nav.pack(fill=tk.BOTH, expand=True)
        
        # Status bar
        self.status = w.StatusBar(self)
        self.status.pack(side=tk.BOTTOM, fill=tk.X)
        
    def create_config(self):
        self._configs_nav.create_config()
        
    def save_configs(self):
        self._configs_nav.save_configs()
        
    def load_configs(self):
        self._configs_nav.load_configs()
        
    def validate_configs(self):
        self._configs_nav.validate_configs()                     
       
    def help_about(self):
        d = AboutDialog(self)

        self.wait_window(d)
        
    def quit(self):
        root.quit()
        
class SchemasConfigurator(tk.Frame):
    def __init__(self, parent, schemas=None):
        
        if schemas is None:
            schemas = conf.ConfigurationSchema.configuration_schemas()
        
        tk.Frame.__init__(self, parent, relief=tk.SUNKEN)
        
        parent.title('Schemas configurator')
        
        # Menubar
        self.menu_bar = tk.Menu(self)
        
        # Schemas menu
        schemas_menu = tk.Menu(self.menu_bar)
        schemas_menu.add_command(label="New", command=self.create_schema)
        schemas_menu.add_command(label="Save", command=self.save_schemas)
        schemas_menu.add_command(label="Load", command=self.load_schemas)
        self.menu_bar.add_cascade(label='Schemas', menu=schemas_menu)
        
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
            
        self._schemas_nav = ConfigurationSchemaNavigator(self, schemas)
         
        self._schemas_nav.pack(fill=tk.BOTH, expand=True, padx=2, pady=3)
        
        # Status bar
        self.status = w.StatusBar(self)
        self.status.pack(side=tk.BOTTOM, fill=tk.X)
        
    def create_schema(self):
        self._schemas_nav.create_schema()
        
    def save_schemas(self):
        self._schemas_nav.save_schemas()
        
    def load_schemas(self):
        self._schemas_nav.load_schemas()
        
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
    
    parser = argparse.ArgumentParser(description='Configurator. Configuration management utility.')
    parser.add_argument('-f', '--full', help='Run the full configurator (both configurations and schemas navigation)', action='store_true')
    parser.add_argument('-s', '--schemas', help='The configuration schemas files. Default is configurator.schema')
    parser.add_argument('-c', '--configs', help='The configurations file. Default is configurator.config')
    parser.add_argument('--setup', help='Edit configuration schemas', action='store_true')
    parser.add_argument('--debug', help='Run in debug mode', action='store_true')
    args = parser.parse_args()
    
    schemas_file = None
    
    if args.schemas is not None:
        if os.path.exists(args.schemas):
            schemas_file = args.schemas
        elif os.path.exists(os.getcwd() + '/' + args.schemas):
            schemas_file = os.getcwd() + '/' + args.schemas
        else:
            sys.exit('Schema file ' + args.schemas + ' does not exist')
        
    if schemas_file is None:
        schemas_file = os.getcwd() + '/configurator.schema'
    
    # Try to load the schemas
    schemas = []
    if  os.path.exists(schemas_file):
        unserializer = conf.ConfigurationSchemasXMLUnserializer()
        schemas = unserializer.read(schemas_file)
    
    configs_file = None
    if args.configs is not None:
        if os.path.exists(args.config):
            configs_file = args.configs
        elif os.path.exists(os.getcwd() + '/' + args.config):
            configs_file = os.path.exists(os.getcwd() + '/' + args.config)
        else:
            sys.exit('Configuration file ' + args.configs + ' does not exist')     
    
    if configs_file is None:
        configs_file = os.getcwd() + '/configurator.config'
            
    # Try loading the configurations
    configs = []
    
    if os.path.exists(configs_file):
        unserializer = conf.ConfigurationsXMLUnserializer()
        configs = unserializer.read(configs_file)        
        
    root = tk.Tk()
    
    if args.full:
        configurator = FullConfigurator(root, configs=configs)
    elif args.setup:
        configurator = SchemasConfigurator(root)
    else:
        configurator = Configurator(root, configs=configs)
    
    configurator.pack(fill=tk.BOTH, expand=True)
    root.mainloop()
