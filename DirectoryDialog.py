"""Directory selection dialog classes.

Classes:

- DirectoryDialog

History: created from the standard FileDialog class.

$Id: DirectoryDialog.py,v 80fe4c62544d 2005/03/25 14:37:35 simon $

"""

from Tkinter import *
from Dialog import Dialog

import os
import fnmatch


dialogstates = {}


class DirectoryDialog:

    """Standard directory selection dialog.

    Usage:

        d = DirectoryDialog(master)
        dir = d.go(dir_or_file, key)
        if dir is None: ...cancelled...
        else: ...do stuff in directory...

    All arguments to go() are optional.

    The 'key' argument specifies a key in the global dictionary
    'dialogstates', which keeps track of the values for the directory
    argument, overriding the value passed in.  If no key is specified,
    the dialog keeps no memory of previous state.  Note that memory is
    kept even when the dialog is canceled.  (All this emulates the
    behavior of the Macintosh file selection dialogs.)

    """

    title = "Directory Selection Dialog"

    def __init__(self, master, title=None):
        if title is None: title = self.title
        self.master = master
        self.directory = None

        self.top = Toplevel(master)
        self.top.title(title)
        self.top.iconname(title)

        self.botframe = Frame(self.top)
        self.botframe.pack(side=BOTTOM, fill=X)

        self.selection = Entry(self.top)
        self.selection.pack(side=BOTTOM, fill=X)
        self.selection.bind('<Return>', self.ok_event)

        self.filter = Entry(self.top)
        self.filter.pack(side=TOP, fill=X)
        self.filter.bind('<Return>', self.filter_command)

        self.midframe = Frame(self.top)
        self.midframe.pack(expand=YES, fill=BOTH)

#        self.filesbar = Scrollbar(self.midframe)
#        self.filesbar.pack(side=RIGHT, fill=Y)
#        self.files = Listbox(self.midframe, exportselection=0,
#                             yscrollcommand=(self.filesbar, 'set'))
#        self.files.pack(side=RIGHT, expand=YES, fill=BOTH)
#        btags = self.files.bindtags()
#        self.files.bindtags(btags[1:] + btags[:1])
#        self.files.bind('<ButtonRelease-1>', self.files_select_event)
#        self.files.bind('<Double-ButtonRelease-1>', self.files_double_event)
#        self.filesbar.config(command=(self.files, 'yview'))

        self.dirsbar = Scrollbar(self.midframe)
        self.dirsbar.pack(side=RIGHT, fill=Y)
        self.dirs = Listbox(self.midframe, exportselection=0,
                            yscrollcommand=(self.dirsbar, 'set'))
        self.dirs.pack(side=LEFT, expand=YES, fill=BOTH)
        self.dirsbar.config(command=(self.dirs, 'yview'))
        btags = self.dirs.bindtags()
        self.dirs.bindtags(btags[1:] + btags[:1])
        self.dirs.bind('<ButtonRelease-1>', self.dirs_select_event)
        self.dirs.bind('<Double-ButtonRelease-1>', self.dirs_double_event)

        self.ok_button = Button(self.botframe,
                                text="OK",
                                command=self.ok_command)
        self.ok_button.pack(side=LEFT)
#        self.filter_button = Button(self.botframe,
#                                    text="Filter",
#                                    command=self.filter_command)
#        self.filter_button.pack(side=LEFT, expand=YES)
        self.cancel_button = Button(self.botframe,
                                    text="Cancel",
                                    command=self.cancel_command)
        self.cancel_button.pack(side=RIGHT)

        self.top.protocol('WM_DELETE_WINDOW', self.cancel_command)
        # XXX Are the following okay for a general audience?
        self.top.bind('<Alt-w>', self.cancel_command)
        self.top.bind('<Alt-W>', self.cancel_command)

    def go(self, dir=os.curdir):
#        if key and dialogstates.has_key(key):
#            self.directory, pattern = dialogstates[key]
#        else:
        dir = os.path.expanduser(dir)
        if os.path.isdir(dir):
            self.directory = dir
        else:
            self.directory, default = os.path.split(dir)
        self.set_filter(self.directory, 0)
        self.set_selection('')
        self.filter_command()
        self.selection.focus_set()
        self.top.grab_set()
        self.how = None
        self.master.mainloop()          # Exited by self.quit(how)
#        if key:
        directory, pattern = self.get_filter()
        if self.how:
            directory = os.path.dirname(self.how)
#            dialogstates[key] = directory, pattern
        self.top.destroy()
        return self.how

    def quit(self, how=None):
        self.how = how
        self.master.quit()              # Exit mainloop()

    def dirs_double_event(self, event):
        self.filter_command()

    def dirs_select_event(self, event):
        dir, pat = self.get_filter()
        subdir = self.dirs.get('active')
        dir = os.path.normpath(os.path.join(self.directory, subdir))
        self.set_filter(dir, pat)

#    def files_double_event(self, event):
#        self.ok_command()

#    def files_select_event(self, event):
#        file = self.files.get('active')
#        self.set_selection(file)

    def ok_event(self, event):
        self.ok_command()

    def ok_command(self):
        self.quit(self.get_selection())

    def filter_command(self, event=None):
        dir, pat = self.get_filter()
        try:
            names = os.listdir(dir)
        except os.error:
            self.master.bell()
            return
        self.directory = dir
        self.set_filter(dir, pat)
        names.sort()
        subdirs = [os.pardir]
#        matchingfiles = []
        for name in names:
            fullname = os.path.join(dir, name)
            if os.path.isdir(fullname):
                subdirs.append(name)
#            elif fnmatch.fnmatch(name, pat):
#                matchingfiles.append(name)
        self.dirs.delete(0, END)
        for name in subdirs:
            self.dirs.insert(END, name)
#        self.files.delete(0, END)
#        for name in matchingfiles:
#            self.files.insert(END, name)
        head, tail = os.path.split(self.get_selection())
        if tail == os.curdir: tail = ''
        self.set_selection(tail)

    def get_filter(self):
        filter = self.filter.get()
        filter = os.path.expanduser(filter)
        if filter[-1:] == os.sep or os.path.isdir(filter):
            filter = os.path.join(filter, "*")
        return os.path.split(filter)

    def get_selection(self):
        file = self.selection.get()
        file = os.path.expanduser(file)
        return file

    def cancel_command(self, event=None):
        self.quit()

    def set_filter(self, dir, pat):
        if not os.path.isabs(dir):
            try:
                pwd = os.getcwd()
            except os.error:
                pwd = None
            if pwd:
                dir = os.path.join(pwd, dir)
                dir = os.path.normpath(dir)
        self.filter.delete(0, END)
        self.filter.insert(END, os.path.join(dir or os.curdir, "*"))

    def set_selection(self, file):
        self.selection.delete(0, END)
        self.selection.insert(END, os.path.join(self.directory, file))


def test():
    """Simple test program."""
    root = Tk()
    root.withdraw()
    dd = DirectoryDialog(root)
    dir = dd.go()
    print dir


if __name__ == '__main__':
    test()
