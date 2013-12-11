import Tkinter as tk
import ttk

class DoubleListSelector(tk.Frame):
    def __init__(self, master, **options):
        tk.Frame.__init__(self, master)
        
        self.source = options.get("source")
        self.selected = options.get("selected")
        
        self.source_list_var = tk.StringVar()
        self.source_list = tk.Listbox(self, exportselection=0, listvar=self.source_list_var, selectmode=tk.MULTIPLE)
        self.source_list.bind('<ButtonRelease-1>', self.source_list_changed)
        
        for item in self.source:
            if not item in self.selected:
                self.source_list.insert(tk.END, str(item))
                
        self.selected_list_var = tk.StringVar()
        self.selected_list = tk.Listbox(self, exportselection=0, listvar=self.selected_list_var, selectmode=tk.MULTIPLE)
        self.selected_list.bind('<ButtonRelease-1>', self.selected_list_changed)
        
        for item in self.selected:
            self.selected_list.insert(tk.END, str(item))
            
        buttons = tk.Frame(self)
        self.add_btn = tk.Button(buttons, text="Add", command=self.select_item, state=tk.DISABLED)
        self.add_btn.pack()
        
        self.remove_btn = tk.Button(buttons, text="Remove", command=self.unselect_item, state=tk.DISABLED)
        self.remove_btn.pack()
        
        self.selected_list.pack(side=tk.LEFT)
        buttons.pack(side=tk.LEFT)
        self.source_list.pack(side=tk.LEFT)
    
    def source_list_changed(self, ev):
        if self.source_list.curselection():
            self.add_btn.configure(state=tk.NORMAL)
        else:
            self.add_btn.configure(state=tk.DISABLED)
            
    def selected_list_changed(self, ev):
        if self.selected_list.curselection():
            self.remove_btn.configure(state=tk.NORMAL)
        else:
            self.remove_btn.configure(state=tk.DISABLED)
            
    def select_item(self):
        selected_items = []
        for i in self.source_list.curselection():
            item = self.source[int(i)]
            selected_items.append(item)
        
        for item in selected_items:
            self.source.remove(item)
               
        self.selected.extend(selected_items)
        self.update_listboxes()
        self.add_btn.configure(state=tk.DISABLED)
        
    def update_listboxes(self):
        self.selected_list_var.set(' '.join(map(str, self.selected)))
        self.source_list_var.set(' '.join(map(str, self.source)))
        
    def unselect_item(self):
        selected_items = []
        for i in self.selected_list.curselection():
            item = self.selected[int(i)]
            selected_items.append(item)
        
        for item in selected_items:
            self.selected.remove(item)
            
        self.source.extend(selected_items)
        self.update_listboxes()
        self.remove_btn.configure(state=tk.DISABLED)
                  
        
configuration_schemas = {}

def get_configuration_schema(name):
    global configuration_schemas
    return configuration_schemas[name]

def register_configuration_schema(schema):
    global configuration_schemas
    configuration_schemas[schema.name] = schema
    
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
        return self
    
    def sections(self):
        return self._sections.values()
    
    def get_section(self, name):
        return self._sections[name]
    
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
    
    def __str__(self):
        return self.name 
        
class ConfigurationSchemaSection:
    def __init__(self, name, **args):
        self._name = name
        self._subsections = {}
        self._options = {}
        self._documentation = args.get('documentation') or 'Not documented'
        
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
        return self
        
    def get_option(self, name):
        return self._options[name]
        
    @property
    def documentation(self):
        return self._documentation
    
    @documentation.setter
    def documentation(self, value):
        self._documentation = value
        return self

class ConfigurationSchemaOption:
    def __init__(self, name, option_type, **args):
        self._name = name
        self._option_type = option_type
        self._documentation = args.get('documentation') or 'Not documented'
    
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
    
class ConfigurationSchemaNavigator(tk.Frame):
    def __init__(self, master, schemas):
        tk.Frame.__init__(self, master)
        
        self.schemas = schemas
        
        # The pane
        pane = tk.PanedWindow(self, orient=tk.HORIZONTAL)
        
        # The tree
        self.tree = ttk.Treeview(pane)
        ysb = ttk.Scrollbar(self, orient='vertical', command=self.tree.yview)
        xsb = ttk.Scrollbar(self, orient='horizontal', command=self.tree.xview)
        self.tree.configure(yscroll=ysb.set, xscroll=xsb.set)
        self.tree.heading('#0', text='Configuration schemas', anchor='w')
        
        for schema in schemas:
            self.insert_schema(schema)
            
        self.tree.tag_bind('schema', '<ButtonRelease-1>', self.select_schema)
        self.tree.tag_bind('section', '<ButtonRelease-1>', self.select_section)
        self.tree.tag_bind('option', '<ButtonRelease-1>', self.select_option)
        
        pane.add(self.tree)
        
        # The editor
        
        self.editor = ConfigurationSchemaEditor(pane, schemas[0])
        pane.add(self.editor)
        pane.pack()
        
    def select_schema(self, ev):
        item_id = str(self.tree.focus())
        schema = self.find_schema(item_id)
        print 'Selected schema was %s' % item_id
        print schema
        
    def find_schema(self, id):
        return next((sc for sc in self.schemas if sc.name == id), None)

    def select_section(self, ev):
        item_id = str(self.tree.focus())
        print 'Selected section was %s' % item_id
        section = self.find_section(item_id)
        print section
        
    def find_section(self, id):
        path = id.split('.')
        # First element is the schema, find it first
        schema = self.find_schema(path[0])
        
        section = schema
        i = 1
        while i <= len(path) - 1:
            section = section.get_section(path[i])
            i = i + 1
        
        return section
    
    def select_option(self, ev):
        item_id = str(self.tree.focus())
        print 'Selected option was %s' % item_id
        print self.find_option(item_id)
        
    def find_option(self, id):
        path= id.split('.')
        schema = self.find_schema(path[0])
        section = schema
        i = 1
        while i <= len(path) - 2:
            section = section.get_section(path[i])
            i = i + 1
        return section.get_option(path[len(path) - 1])
        
    def insert_schema(self, schema):
        sc = self.tree.insert('', 'end', schema.name, text=schema.name, tags='schema')
        for section in schema.sections():
            self.insert_section(section, sc)
            
    def insert_section(self, section, parent):
        sc = self.tree.insert(parent, 'end', parent + '.' + section.name, text=section.name, tags='section')
        for subsection in section.subsections():
            self.insert_section(subsection, sc)
        
        for option in section.options():
            self.tree.insert(sc, 'end', sc + '.' + option.name, text=option.name, tags='option')
            
class ConfigurationSchemaEditor(tk.Frame):
    def __init__(self, master, schema):
        tk.Frame.__init__(self, master)
        
        self.schema = schema
        tk.Label(self, text="Name: ").grid(row=0, column=0)
        self.schema_name = tk.StringVar()
        self.schema_name.set(schema.name or "")
        
        tk.Entry(self, textvariable=self.schema_name).grid(row=0, column=1)
        tk.Label(self, text="Parents:").grid(row=1, column=0)
        parents = DoubleListSelector(self, source=list_configuration_schemas(),
                                        selected=schema.parents())
        parents.grid(row=1, column=1)
        
        self.schema_documentation = schema.documentation
            
        tk.Label(self, text="Documentation").grid(row=2, column=0)
        text = tk.Text(self)
        text.insert(tk.END, self.schema_documentation)
        text.grid(row=2, column=1)
        
class ConfigurationSchemaSectionEditor(tk.Frame):
    def __init__(self, master, section):
        tk.Frame.__init__(self, master)
        
        self.section = section
        tk.Label(self, text="Name: ").grid(row=0, column=0)
        tk.Entry()                
           
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

if __name__ == '__main__':
    schemas = []
    sch1 = ConfigurationSchema("Web")
    host = ConfigurationSchemaOption("host", 'uri', documentation="Server host")
    s1 = ConfigurationSchemaSection("Server").add_option(host)
    sch1.section(s1)
    schemas.append(sch1)
    
    db = ConfigurationSchema("Database")
    db_engine = ConfigurationSchemaOption("engine", "option", documentation="The database engine")
    db_server = ConfigurationSchemaSection("Server").add_option(db_engine)
    db.section(db_server)
    schemas.append(db)
    
    root = tk.Tk()
    navigator = ConfigurationSchemaNavigator(root, schemas)
    navigator.pack()
    root.mainloop()