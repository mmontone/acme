import Tkinter as tk
import ttk
import widgets as w
import configuration as conf
import tkMessageBox
    
class ConfigurationSchemaNavigator(tk.Frame):
    def __init__(self, master, schemas):
        tk.Frame.__init__(self, master)
        
        self.schemas = schemas
        self.items = {}
        
        # The pane
        self.pane = tk.Frame(self)
        
        # The tree
        self.tree = ttk.Treeview(self.pane)
        ysb = ttk.Scrollbar(self.pane, orient='vertical', command=self.tree.yview)
        xsb = ttk.Scrollbar(self.pane, orient='horizontal', command=self.tree.xview)
        self.tree.configure(yscroll=ysb.set, xscroll=xsb.set)
        self.tree.heading('#0', text='Configuration schemas', anchor='w')
        
        self.tree.bind('<Leave>', lambda ev:configurator.status.set(''))
                
        for schema in schemas:
            self.insert_schema(schema)
            
        # Popup menus
        self.tree.tag_bind('schema', '<ButtonRelease-1>', self.select_schema)
        self.tree.tag_bind('schema', '<ButtonRelease-3>', self.popup_schema)
        
        self.tree.tag_bind('section', '<ButtonRelease-1>', self.select_section)
        self.tree.tag_bind('section', '<ButtonRelease-3>', self.popup_section)
        
        self.tree.tag_bind('option', '<ButtonRelease-1>', self.select_option)
        self.tree.tag_bind('option', '<ButtonRelease-3>', self.popup_option)
                
        #self.tree.bind('<ButtonRelease-3>', self.tree_popup)
        
        #Configuration
        #print(self.tree.tag_configure('schema'))
        #self.tree.tag_configure('schema', font=('Helvetica', '16'))
        
        self.tree.grid(column=0, row=0, sticky=tk.N+tk.S)
        
        # The editor
        self.editor = ConfigurationSchemaEditor(self.pane, schemas[0])
        self.editor.grid(column=1, row=0)
        self.pane.pack(fill=tk.BOTH, expand=True)
        
    def tree_popup(self, ev):
        popup = tk.Menu(self, tearoff=0)
        popup.add_command(label="New configuration schema") # , command=next) etc...
               
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
        # create a menu
        popup = tk.Menu(self, tearoff=0)
        popup.add_command(label="Remove") # , command=next) etc...
        popup.add_command(label="Add option")
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
            
        self.editor.grid_forget()
        self.editor = ConfigurationSchemaSectionCreator(self.pane, onsave=save_section)
        self.editor.grid(column=1, row=0)
    
    def select_option(self, ev):
        item_id = str(self.tree.focus())
        print 'Selected option was %s' % item_id
        option = self.find_option(item_id)
        print self.find_option(item_id)
        self.editor.grid_forget()
        self.editor = ConfigurationSchemaOptionEditor(self.pane, option)
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
                
class ConfigurationSchemaSectionCreator(tk.Frame):
    def __init__(self, master, **options):
        tk.Frame.__init__(self, master)
        
        # configuration
        self.section = conf.ConfigurationSchemaSection()
        self._onsave = options.get('onsave') or None
                
        # ui
        tk.Label(self, text="New section").pack()
        
        f = tk.Frame(self)
        
        tk.Label(f, text="Name: ").grid(row=0, column=0, sticky=tk.W)
        self.section_name = tk.StringVar()
        tk.Entry(f, textvariable=self.section_name).grid(row=0, column=1, sticky=tk.W)
        
        tk.Label(f, text="Documentation").grid(row=1, column=0, sticky=tk.W)
        self.section_documentation = tk.Text(f, height=10, width=60)
        self.section_documentation.grid(row=1, column=1, sticky=tk.W)
        
        buttons = tk.Frame(f)
        
        save = tk.Button(buttons, text="Save", command=self.save_section)
        save.pack(side=tk.LEFT, padx=2)
        set_status_message(save, "Create the new section")
               
        buttons.grid(row=2, column=1, sticky=tk.SE)
        
        f.pack()
        
    def save_section(self):
        self.section.name = self.section_name.get()
        self.section.documentation = self.section_documentation.get(1.0, tk.END)
        configurator.status.set(self.section.name + " section has been created")
        
        if self._onsave:
            self._onsave(self.section)
                   
class ConfigurationSchemaOptionEditor(tk.Frame):
    def __init__(self, master, option):
        tk.Frame.__init__(self, master)
        
        self.option = option
        
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
        option_types = map(lambda o: o.option_name(), conf.OptionType.__subclasses__())
        tk.OptionMenu(self.f, self.option_type, *option_types, command=self.edit_option_type).grid(row=1, column=1, sticky=tk.W) 
        
        if option.option_type:
            editor = OptionTypeEditor.for_option_type(option.option_type.__class__)
            if editor:
                self.option_type_editor = editor(self.f, option.option_type)
            else:
                self.option_type_editor = tk.Frame(self.f)
        else:
            self.option_type_editor = tk.Frame(self.f)
            
        self.option_type_editor.grid(row=2, column=1, sticky=tk.W)
        
        # Required?
        tk.Label(self.f, text="Is required?: ").grid(row=3, column=0, sticky=tk.W)
        self.option_required = tk.IntVar()
        self.option_required.set(1 if option.is_required else 0)
        required = tk.Checkbutton(self.f, variable=self.option_required)
        set_status_message(required, "Whether the option is required. If the option is required, then it is mandatory to set its value")
        required.grid(row=3, column=1, sticky=tk.W)
        
        
        # Documentation
        self.option_documentation = option.documentation
        tk.Label(self.f, text="Documentation:").grid(row=4, column=0, sticky=tk.W)
        text = tk.Text(self.f, width=60, height=10)
        text.insert(tk.END, self.option_documentation)
        text.grid(row=4, column=1, sticky=tk.W)
        
        buttons = tk.Frame(self.f)
        tk.Button(buttons, text="Save").pack(side=tk.LEFT, padx=2)
        tk.Button(buttons, text="Remove").pack(side=tk.LEFT, padx=2)
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
        
        configs = tk.Frame(self)
        
        self.tree = ttk.Treeview(configs)
        ysb = ttk.Scrollbar(configs, orient='vertical', command=self.tree.yview)
        xsb = ttk.Scrollbar(configs, orient='horizontal', command=self.tree.xview)
        self.tree.configure(yscroll=ysb.set, xscroll=xsb.set)
        self.tree.heading('#0', text='Configuration', anchor='w')

        #abspath = os.path.abspath(path)
        #root_node = self.tree.insert('', 'end', text="Configuration schemas", open=True)
        #self.process_directory(root_node, abspath)

        self.tree.grid(row=0, column=0)
        ysb.grid(row=0, column=1, sticky='ns')
        xsb.grid(row=1, column=0, sticky='ew')
        
        self.tree.bind("<Button-1>", self.onDoubleClick)
        configs.pack(side=tk.LEFT)
        
        self.panel = tk.Frame(self)
        tk.Label(self.panel, text="Hello!!").pack()
        self.panel.pack()
                
        self.grid()
        
    def onDoubleClick(self, event):
        if isinstance(self.tree.selection(), tuple):
            item = self.tree.selection()[0] 
            print "You clicked on ", self.tree.item(item, "text")
            self.panel.pack_forget()
            self.panel= tk.Frame(self)
            
            self.panel.pack()
            tk.Label(self.panel, text=("You clicked on ", self.tree.item(item, "text"))).pack()
            

    def process_directory(self, parent, path):
        for p in os.listdir(path):
            abspath = os.path.join(path, p)
            isdir = os.path.isdir(abspath)
            oid = self.tree.insert(parent, 'end', text=p, open=False)
            if isdir:
                self.process_directory(oid, abspath)
                
class AboutDialog(tk.Toplevel):

    def __init__(self, parent):
        
        tk.Toplevel.__init__(self, parent)

        self.transient(parent)
        self.title("About configurator")
        
        self.geometry("+%d+%d" % (parent.winfo_rootx()+50,
                                  parent.winfo_rooty()+50))

        tk.Label(self, text="This is configurator, a tool for managing application configurations." +
                            "\n\n Home page: https://github.com/mmontone/configurator" +  
                            "\n\n Author: Mariano Montone").pack()

        b = tk.Button(self, text="OK", command=self.ok)
        b.pack(pady=5)

    def ok(self):

        self.destroy()
    
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
        
        self.menu_bar.add_command(label='Quit', command=self.quit)
        
        try:
            parent.config(menu=self.menu_bar)
        except AttributeError:
            # master is a toplevel window (Python 1.4/Tkinter 1.63)
            parent.tk.call(parent, "config", "-menu", menu_bar)
            
        # Tabs
        tabs = ttk.Notebook(self, name='notebook')
        tabs.enable_traversal()
        
        configs_nav = self.init_configs_navigator()
        tabs.add(configs_nav, text='Configurations')
        
        navigator = self.init_schemas_navigator()
        tabs.add(navigator, text='Configuration schemas')
                      
        tabs.pack(fill=tk.BOTH, expand=tk.Y, padx=2, pady=3)
        
        # Status bar
        self.status = w.StatusBar(self)
        self.status.pack(side=tk.BOTTOM, fill=tk.X)
        
    def init_schemas_navigator(self):
        schemas = []
        
        sch1 = conf.ConfigurationSchema("Web")
        host = conf.ConfigurationSchemaOption("host", conf.StringOptionType(), documentation="Server host")
        s1 = conf.ConfigurationSchemaSection("Server").add_option(host)
        sch1.section(s1)
        schemas.append(sch1)
    
        db = conf.ConfigurationSchema("Database")
        db_engine = conf.ConfigurationSchemaOption("engine", conf.ChoiceOptionType(["Postgresql", "Mysql"]), documentation="The database engine")
        db_engine.is_required = True
        db_server = conf.ConfigurationSchemaSection("Server").add_option(db_engine)
        db.section(db_server)
        schemas.append(db)
        
        return ConfigurationSchemaNavigator(self, schemas)
    
    def init_configs_navigator(self):
        return self.init_schemas_navigator()
                        
       
    def help_about(self):
        d = AboutDialog(self)

        self.wait_window(d)
        
    def quit(self):
        root.quit()
        
def set_status_message(widget, message):
    global configurator
    widget.bind('<Enter>', lambda ev:configurator.status.set(message))
    widget.bind('<Leave>', lambda ev:configurator.status.set(''))
        
if __name__ == '__main__':
    root = tk.Tk()
    configurator = Configurator(root)
    configurator.pack(fill=tk.BOTH, expand=True)
    root.mainloop()