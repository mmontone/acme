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
        
class ListEditor(tk.Frame):
    def __init__(self, parent, **options):
        tk.Frame.__init__(self, parent)
        
        self.list_var = options.get('listvar')
        self.list = tk.Listbox(self, exportselection=0, listvar=self.list_var, selectmode=tk.MULTIPLE)
        self.list.pack(side=tk.LEFT)
        actions = tk.Frame(self)
        tk.Button(actions, text="Remove", command=self.remove_item).pack()
        self.new_item = tk.StringVar()
        tk.Entry(actions, textvariable=self.new_item).pack()
        tk.Button(actions, text="Add", command=self.add_item).pack()
        actions.pack()
           
    def add_item(self):
        if self.new_item.get():
            items = self.list_var.get().split(',')
            print items
            items.append(self.new_item.get())
            self.list_var.set(' '.join(items))
    
    def remove_item(self):
        items = self.list_var.get().split(',')
        new_items = list(items)
        for i in map(int, self.list.curselection()):
            new_items.remove(items[i])
            
        self.list_var.set(' '.join(new_items))