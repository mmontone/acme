import Tkinter as tk
import ttk
import widgets as w
import configuration as conf
    
class ConfigurationSchemaNavigator(tk.Frame):
    def __init__(self, master, schemas):
        tk.Frame.__init__(self, master)
        
        self.schemas = schemas
        
        # The pane
        self.pane = tk.Frame(self)
        
        # The tree
        self.tree = ttk.Treeview(self.pane)
        ysb = ttk.Scrollbar(self.pane, orient='vertical', command=self.tree.yview)
        xsb = ttk.Scrollbar(self.pane, orient='horizontal', command=self.tree.xview)
        self.tree.configure(yscroll=ysb.set, xscroll=xsb.set)
        self.tree.heading('#0', text='Configuration schemas', anchor='w')
                
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
        self.pane.pack()
        
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
        self.editor = ConfigurationSchemaEditor(self.pane, schema)
        self.editor.grid(column=1, row=0)
        
    def popup_schema(self, ev):
        # create a menu
        popup = tk.Menu(self, tearoff=0)
        popup.add_command(label="Remove") # , command=next) etc...
        popup.add_command(label="Add section")
        
        # display the popup menu
        try:
            popup.tk_popup(ev.x_root, ev.y_root, 0)
        finally:
            # make sure to release the grab (Tk 8.0a1 only)
            popup.grab_release()
               
    def find_schema(self, id):
        return next((sc for sc in self.schemas if sc.name == id), None)

    def select_section(self, ev):
        item_id = str(self.tree.focus())
        print 'Selected section was %s' % item_id
        section = self.find_section(item_id)
        print section
        self.editor.grid_forget()
        self.editor = ConfigurationSchemaSectionEditor(self.pane, section)
        self.editor.grid(column=1, row=0)
        
    def popup_section(self, ev):
        # create a menu
        popup = tk.Menu(self, tearoff=0)
        popup.add_command(label="Remove") # , command=next) etc...
        popup.add_command(label="Add option")
        
        # display the popup menu
        try:
            popup.tk_popup(ev.x_root, ev.y_root, 0)
        finally:
            # make sure to release the grab (Tk 8.0a1 only)
            popup.grab_release()
        
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
        option = self.find_option(item_id)
        print self.find_option(item_id)
        self.editor.grid_forget()
        self.editor = ConfigurationSchemaOptionEditor(self.pane, option)
        self.editor.grid(column=1, row=0)
        
    def popup_option(self, ev):
        # create a menu
        popup = tk.Menu(self, tearoff=0)
        popup.add_command(label="Remove") # , command=next) etc...
                
        # display the popup menu
        try:
            popup.tk_popup(ev.x_root, ev.y_root, 0)
        finally:
            # make sure to release the grab (Tk 8.0a1 only)
            popup.grab_release()
            
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
        tk.Label(self, text=schema.name + " configuration schema").pack()
        props = tk.Frame(self)
        tk.Label(props, text="Name: ").grid(row=0, column=0)
        self.schema_name = tk.StringVar()
        self.schema_name.set(schema.name or "")
        
        tk.Entry(props, textvariable=self.schema_name).grid(row=0, column=1)
        tk.Label(props, text="Parents:").grid(row=1, column=0)
        parents = w.DoubleListSelector(props, source=conf.list_configuration_schemas(),
                                        selected=schema.parents())
        parents.grid(row=1, column=1)
        
        self.schema_documentation = schema.documentation
        tk.Label(props, text="Documentation").grid(row=2, column=0)
        text = tk.Text(props)
        text.insert(tk.END, self.schema_documentation)
        text.grid(row=2, column=1)
        
        buttons = tk.Frame(props)
        tk.Button(buttons, text="Save").pack(side=tk.LEFT)
        tk.Button(buttons, text="Remove").pack(side=tk.LEFT)
        buttons.grid(row=3, column=1, sticky=tk.SE)
        props.pack()
        
class ConfigurationSchemaSectionEditor(tk.Frame):
    def __init__(self, master, section):
        tk.Frame.__init__(self, master)
        
        self.section = section
        
        tk.Label(self, text=section.name + " section").pack()
        
        f = tk.Frame(self)
        
        tk.Label(f, text="Name: ").grid(row=0, column=0)
        self.section_name = tk.StringVar()
        self.section_name.set(section.name or "")
        tk.Entry(f, textvariable=self.section_name).grid(row=0, column=1)
        
        self.section_documentation = section.documentation
        tk.Label(f, text="Documentation").grid(row=1, column=0)
        text = tk.Text(f)
        text.insert(tk.END, self.section_documentation)
        text.grid(row=1, column=1)
        
        buttons = tk.Frame(f)
        tk.Button(buttons, text="Save").pack(side=tk.LEFT)
        tk.Button(buttons, text="Remove").pack(side=tk.LEFT)
        buttons.grid(row=2, column=1, sticky=tk.SE)
        
        f.pack()
        
class ConfigurationSchemaOptionEditor(tk.Frame):
    def __init__(self, master, option):
        tk.Frame.__init__(self, master)
        
        self.option = option
        
        tk.Label(self, text=option.name + " option").pack()
        
        self.f = tk.Frame(self)
        
        tk.Label(self.f, text="Name: ").grid(row=0, column=0)
        self.option_name = tk.StringVar()
        self.option_name.set(option.name or "")
        tk.Entry(self.f, textvariable=self.option_name).grid(row=0, column=1)
        
        tk.Label(self.f, text="Type: ").grid(row=1, column=0)
        self.option_type = tk.StringVar()
        if option.option_type:
            self.option_type.set(option.option_type.name)
        option_types = map(lambda o: o.option_name(), conf.OptionType.__subclasses__())
        tk.OptionMenu(self.f, self.option_type, *option_types, command=self.edit_option_type).grid(row=1, column=1) 
        
        if option.option_type:
            editor = OptionTypeEditor.for_option_type(option.option_type.__class__)
            if editor:
                self.option_type_editor = editor(self.f, option.option_type)
            else:
                self.option_type_editor = tk.Frame(self.f)
        else:
            self.option_type_editor = tk.Frame(self.f)
            
        self.option_type_editor.grid(row=2, column=1)       
        
        self.option_documentation = option.documentation
        tk.Label(self.f, text="Documentation:").grid(row=3, column=0)
        text = tk.Text(self.f)
        text.insert(tk.END, self.option_documentation)
        text.grid(row=3, column=1)
        
        buttons = tk.Frame(self.f)
        tk.Button(buttons, text="Save").pack(side=tk.LEFT)
        tk.Button(buttons, text="Remove").pack(side=tk.LEFT)
        buttons.grid(row=4, column=1, sticky=tk.SE)
        
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
                
class AboutDialog:

    def __init__(self, parent):

        top = self.top = tk.Toplevel(parent)

        tk.Label(top, text="This is configurator, a tool for managing application configurations." +
                            "\n\n Home page: https://github.com/mmontone/configurator" +  
                            "\n\n Author: Mariano Montone").pack()

        b = tk.Button(top, text="OK", command=self.ok)
        b.pack(pady=5)

    def ok(self):

        self.top.destroy()
    
class Configurator(tk.Frame):
    def __init__(self, parent):
        
        tk.Frame.__init__(self, parent, relief=tk.SUNKEN)
        
        parent.title('Configurator')
        
        # Menubar
        self.menu_bar = tk.Menu(self)
        
        help_menu = tk.Menu(self.menu_bar)
        help_menu.add_command(label='About', command=self.help_about)
        
        self.menu_bar.add_cascade(label='Help', menu=help_menu)
        
        try:
            parent.config(menu=self.menu_bar)
        except AttributeError:
            # master is a toplevel window (Python 1.4/Tkinter 1.63)
            parent.tk.call(parent, "config", "-menu", menu_bar)
            
        # Tabs
        tabs = ttk.Notebook(self)
        
        
        schemas = []
        
        sch1 = conf.ConfigurationSchema("Web")
        host = conf.ConfigurationSchemaOption("host", conf.StringOptionType(), documentation="Server host")
        s1 = conf.ConfigurationSchemaSection("Server").add_option(host)
        sch1.section(s1)
        schemas.append(sch1)
    
        db = conf.ConfigurationSchema("Database")
        db_engine = conf.ConfigurationSchemaOption("engine", conf.ChoiceOptionType(["Postgresql", "Mysql"]), documentation="The database engine")
        db_server = conf.ConfigurationSchemaSection("Server").add_option(db_engine)
        db.section(db_server)
        schemas.append(db)
        
        navigator = ConfigurationSchemaNavigator(root, schemas)
        navigator.pack()
        
    def help_about(self):
        d = AboutDialog(self)

        self.wait_window(d.top)
        
if __name__ == '__main__':
    root = tk.Tk()
    configurator = Configurator(root)
    root.mainloop()