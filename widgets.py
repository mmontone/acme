import Tkinter as tk

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
        self.add_btn.pack(fill=tk.X)
        
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
        
    def get_selection(self):
        return self.selected
    
    def set_selection(self, items):
        self.selected_list_var.set('')
        self.source_list_var.set('')
        self.selected = items
        
        for item in self.source:
            if not item in self.selected:
                self.source_list.insert(tk.END, str(item))
                
        for item in self.selected:
            self.selected_list.insert(tk.END, str(item))
            
    def disable(self):
        self.source_list.configure(state=tk.DISABLED)
        self.selected_list.configure(state=tk.DISABLED)
        self.add_btn.configure(state=tk.DISABLED)
        self.remove_btn.configure(state=tk.DISABLED)            
        
class ListEditor(tk.Frame):
    def __init__(self, parent, **options):
        tk.Frame.__init__(self, parent)
        
        self.list_var = options.get('listvar')
        self.list = tk.Listbox(self, exportselection=0, listvar=self.list_var, selectmode=tk.MULTIPLE)
        self.list.pack(side=tk.LEFT)
        actions = tk.Frame(self)
        tk.Button(actions, text="Remove", command=self.remove_item).pack(fill=tk.X)
        self.new_item = tk.StringVar()
        tk.Entry(actions, textvariable=self.new_item).pack(fill=tk.X)
        tk.Button(actions, text="Add", command=self.add_item).pack(fill=tk.X)
        actions.pack()
           
    def add_item(self):
        if self.new_item.get():
            items = []
            if self.list_var.get() <> '':
                items = list(eval(self.list_var.get()))
            items.append(self.new_item.get())
            self.list_var.set(' '.join(items))
    
    def remove_item(self):
        if self.list_var.get() <> '':
            items = list(eval(self.list_var.get()))
            new_items = list(items)
            for i in map(int, self.list.curselection()):
                new_items.remove(items[i])
            
            self.list_var.set(' '.join(new_items))
        
class StatusBar(tk.Frame):

    def __init__(self, master):
        tk.Frame.__init__(self, master)
        self.label = tk.Label(self, bd=1, relief=tk.SUNKEN, anchor=tk.W)
        self.label.pack(fill=tk.X)

    def set(self, format, *args):
        self.label.config(text=format % args)
        self.label.update_idletasks()

    def clear(self):
        self.label.config(text="")
        self.label.update_idletasks()
        
class Dialog(tk.Toplevel):
    def __init__(self, parent):
        tk.Toplevel.__init__(self, parent)
        
        # transient is used to associate this window with a parent window
        self.transient(parent)
        
        #if not self.initial_focus:
        #    self.initial_focus = self

        self.geometry("+%d+%d" % (parent.winfo_rootx()+50,
                                  parent.winfo_rooty()+50))

        self.focus_set()
        self.grab_set()

        #self.center2()
        
    def wait_window(self):
        super(Dialog, self).wait_window(self)
        self.grab_set()
        
    def center(self):
        w = self.winfo_screenwidth()
        h = self.winfo_screenheight()
        size = tuple(int(_) for _ in self.geometry().split('+')[0].split('x'))
        x = w/2 - size[0]/2
        y = h/2 - size[1]/2
        #print "Screen width: " + str(w)
        #print "Screen height: " + str(h)
        #print "Size: " + str(size)
        #print "X: " + str(x)
        #print "Y: " + str(y)
        #print "X2: " + str(self.winfo_width())
        #self.geometry("%dx%d+%d+%d" % (size + (x, y)))
        self.geometry("+%d+%d" % (size + (x, y)))
        
    def center2(self):
        self.withdraw()
        self.update_idletasks()  # Update "requested size" from geometry manager

        x = (self.winfo_screenwidth() - self.winfo_reqwidth()) / 2
        y = (self.winfo_screenheight() - self.winfo_reqheight()) / 2
        self.geometry("+%d+%d" % (x, y))

        # This seems to draw the window frame immediately, so only call deiconify()
        # after setting correct window position
        self.deiconify()

class WidgetsListEditor(tk.Frame):
    def __init__(self, master):
        tk.Frame.__init__(self, master)
        
        self._widgets_list = []
        
        self._widgets = tk.Frame(self)
        self._widgets.pack()
        
        add_btn = tk.Button(self, text='+', command=self.add_widget)
        add_btn.pack()       
       
    def add_widget(self):
        
        widget_container = tk.Frame(self._widgets)
        
        widget = self.create_widget()
        
        self._widgets.append(widget)
        
        widget.pack(side=tk.LEFT)
        
        remove_btn = tk.Button(widget_container, text='-', command=lambda: (widget_container.forget(),
                                                                            self._widgets_list.remove(widget)))
        remove_btn.pack()
        
        widget_container.pack()
        
    def create_widget(self):
        raise NotImplementedError("Implement this method")