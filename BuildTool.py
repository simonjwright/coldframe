"""ColdFrame BuildTool classes.

Copyright Simon Wright <simon@pushface.org>.

$Id: BuildTool.py,v 659e7a8845e4 2005/03/25 14:36:10 simon $

"""

from Tkinter import *
from tkMessageBox import *
from DirectoryDialog import *
from FileDialog import *
import os

class ScrolledListBox(Frame):

    """See Demo/tkinter/matt/cancas-with-scrollbars.py from the Python
    source distribution."""

    def __init__(self, master=None):
        Frame.__init__(self, master)
        self.pack()
        self.createWidgets()

    def createWidgets(self):

        # create the actual list ..
        self.list = Listbox(self)

        # .. and the scrollbars ..
	self.list.scrollX = Scrollbar(self, orient=HORIZONTAL)
	self.list.scrollY = Scrollbar(self, orient=VERTICAL)

	# .. now tie the three together ..
	self.list['xscrollcommand'] = self.list.scrollX.set
	self.list['yscrollcommand'] = self.list.scrollY.set
	self.list.scrollX['command'] = self.list.xview
	self.list.scrollY['command'] = self.list.yview

	# .. and pack them up.
	self.list.scrollX.pack(side=BOTTOM, fill=X)
	self.list.scrollY.pack(side=RIGHT, fill=Y)
	self.list.pack(side=LEFT)


class Domain:

    """Represents a ColdFrame domain.
    
    'name' is the name of the domain.

    'path' is the path (relative to $BASE) to the directory where
    name.gen is to be created and name.impl is to be found.

    'ignore' is true if the domain is information-only.

    'implementation' is true if there is code in name.impl.

    'generate' is true if code is to be generated.

    'serialization' is true if the domain is to be included in the
    list of serializable domains (for creating the analysis
    application)."""

    def __init__(self, name):
        self.name = name
        self.ignore = 0
        self.path = ''
        self.implementation = 1
        self.generate = 1
        self.serialization = 1

    def save(self, file):
        print("<domain>");
        print("<name>%s</name>" % self.name)
        if self.ignore:
            print("<ignore/>")
        else:
            if len(self.path) > 0:
                print("<path>%s</path>" % self.path)
            if self.implementation:
                print("<implementation/>")
            if self.generate:
                print("<generate/>")
            if self.serialization:
                print("<serialization/>")
        print("</domain>");


class DomainView(Frame):

    """Shows an editable view of a Domain."""

    def __init__(self, domain, master=None):
        Frame.__init__(self, master)
        self['relief'] = SUNKEN
        self['borderwidth'] = 2
        self.pack()
        self.domain = domain
        self.master = master
        self.createWidgets()

    def createWidgets(self):
        self.name = Label(self)
        self.name['text'] = self.domain.name
        self.name.pack(side=TOP, anchor=W)

        pathFrame = Frame(self)
        self.pathEntry = Entry(pathFrame)
        self.path=StringVar()
        self.path.set(self.domain.path)
        self.pathEntry['textvariable'] = self.path
        self.pathEntry.pack(side=LEFT)
        self.pathButton = Button(pathFrame)
        self.pathButton['text'] = '...'
        self.pathButton['command'] = self.choose_path
        self.pathButton.pack(side=RIGHT)
        pathFrame.pack(side=TOP, fill=X)

        self.ignoreButton = Checkbutton(self)
        self.ignoreButton['text'] = 'ignore'
        self.ignore = BooleanVar()
        self.ignore.set(self.domain.ignore)
        self.ignoreButton['variable'] = self.ignore
        self.ignoreButton.pack(side=TOP, anchor=W)

        self.implementationButton = Checkbutton(self)
        self.implementationButton['text'] = 'implementation'
        self.implementation = BooleanVar()
        self.implementation.set(self.domain.implementation)
        self.implementationButton['variable'] = self.implementation
        self.implementationButton.pack(side=TOP, anchor=W)

        self.generateButton = Checkbutton(self)
        self.generateButton['text'] = 'generate'
        self.generate = BooleanVar()
        self.generate.set(self.domain.generate)
        self.generateButton['variable'] = self.generate
        self.generateButton.pack(side=TOP, anchor=W)

        self.serializationButton = Checkbutton(self)
        self.serializationButton['text'] = 'serialization'
        self.serialization = BooleanVar()
        self.serialization.set(self.domain.serialization)
        self.serializationButton['variable'] = self.serialization
        self.serializationButton.pack(side=TOP, anchor=W)

        self.applyButton = Button(self)
        self.applyButton['text'] = 'Apply'
        self.applyButton['command'] = self.apply
        self.applyButton.pack(side=BOTTOM)

    def view(self, domain):
        """Reset so that 'domain' is displayed."""
        self.domain = domain
        self.name['text'] = self.domain.name
        self.path.set(self.domain.path)
        self.ignore.set(self.domain.ignore)
        self.implementation.set(self.domain.implementation)
        self.generate.set(self.domain.generate)
        self.serialization.set(self.domain.serialization)

    def choose_path(self):
        dir = DirectoryDialog(self).go()
        if dir != None:
            dir, file = os.path.split(dir)
            self.path.set(dir)

    def apply(self):
        """Apply changes to the viewed domain."""
        self.master.changed()
        self.domain.path = self.path.get()
        self.domain.ignore = self.ignore.get()
        self.domain.implementation = self.implementation.get()
        self.domain.generate = self.generate.get()
        self.domain.serialization = self.serialization.get()


class Domains(Frame):

    def __init__(self, master=None, domains={}):
        """'domains' is a dictionary of Domain keyed by name."""
        Frame.__init__(self, master)
        self.pack()
        self.altered = 0
        self.domains = domains
        self.createWidgets()

    def createWidgets(self):
        self.ls = ScrolledListBox(self)
        self.ls.pack(side=LEFT)
        # remember the actual listbox, for convenience
        self.list = self.ls.list
        # add the key bindings
        self.list.bind('<ButtonRelease-1>', self.select)
        self.list.bind('<ButtonRelease-3>', self.delete)
        # initialize the lise (if any)
        self.setUpList()

    def setUpList(self):
        # get the domain names, sorted
        self.keys = self.domains.keys()
        self.keys.sort()
        # delete the current listbox contents
        self.list.delete(0, END)
        # insert the new contents
        for k in self.keys:
            self.list.insert('end', k)
        # if there are any domains ..
        if len(self.keys) > 0:
            # .. view the first
            self.domain = \
                        DomainView(master=self,
                                   domain=self.domains[self.keys[0]])
            self.domain.pack(side=LEFT)

    def changed(self):
        self.altered = 1

    def has_changed(self):
        return self.altered

    def select(self, e):
        try:
            # find the index of the current selection
            sel = e.widget.curselection()[0]
            # view it
            self.domain.view(self.domains[self.list.get(sel)])
        except IndexError:
            # outside any selectable item
            pass

    def delete(self, e):
        try:
            # find the index of the current selection
            sel = e.widget.curselection()[0]
            # check if it's OK to delete it
            if askokcancel("Delete domain", "deleting " + self.list.get(sel)):
                # we've changed
                self.changed()
                # remove from the dictionary of domains
                del self.domains[self.list.get(sel)]
                # delete the domain view
                self.domain.destroy()
                # recreate the list
                self.setUpList()
        except IndexError:
            # outside any selectable item
            pass

    def save(self, file):
        print("<domains>")
        for d in self.domains.values():
            d.save(file)
        print("</domains>")

class Application(Frame):
        
    def __init__(self, master=None):
        """'domains' is a dictionary of Domain keyed by name."""
        Frame.__init__(self, master)
        self.pack()
        self.createWidgets()

    def createWidgets(self):
        
        mBar = Frame(self, relief=RAISED, borderwidth=2)
        mBar.pack(fill=X)

        # make menu button : "File"
        File_button = Menubutton(mBar, text='File', underline=0)
        File_button.pack(side=LEFT, padx="1m")
        File_button.menu = Menu(File_button)
    
        File_button.menu.add_command(label='New...',
                                     underline=0, 
                                     command=self.new_file)
        
        File_button.menu.add_command(label='Open...',
                                     underline=0, 
                                     command=self.open_file)
        
        File_button.menu.add_command(label='Save',
                                     underline=0, 
                                     command=self.save_file)
        
        File_button.menu.add_command(label='Save As...',
                                     underline=5, 
                                     command=self.save_as_file)
        
        File_button.menu.add_command(label='Quit',
                                     underline=0, 
                                     command=self.exit)

        # set up a pointer from the file menubutton back to the file menu
        File_button['menu'] = File_button.menu

        self.domains = Domains(self)
        self.domains.pack(side=TOP)

    def new_file(self):
        pass

    def open_file(self):
        self.domains.destroy()
        ds = {}
        for d in  ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
                   'Quite_A_Long_Name_For_A_Domain']:
            ds[d] = Domain(d)
        self.domains = Domains(self, ds)

    def save_file(self):
        self.domains.save(0)

    def save_as_file(self):
        file = SaveFileDialog(self).go(pattern="*.xml")
        if file != None:
            self.domains.save(file)

    def exit(self):
        if self.domains.has_changed():
            print("something changed")
        self.quit()


app = Application()
app.master.title('ColdFrame build tool')
app.master.iconname('CF build tool')
app.mainloop()
