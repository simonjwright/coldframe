-- $Id: library-test.ts,v cf4daf4ea64c 2001/05/05 16:59:48 simon $

-- A TG test script to check ColdFrame's navigation of Associations.

error_handling
        continue

context
        with Ada.Text_IO; use Ada.Text_IO;
	with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
        with ColdFrame.Exceptions;

        with library.book.collections;
        with library.borrower.collections;
        with library.current_loan.collections;
        with library.loan_history.collections;

        with library.authorship;
        with library.current;
        with library.history;

define
        function "+" (S : String) return Unbounded_String
           renames To_Unbounded_String;

        alice, bob, carol, dave : borrower.handle;
        glorious_deeds, dark_doings, mysterious_happenings  : book.handle;
        cl : current_loan.handle;
        lh : loan_history.handle;

code
        alice := borrower.create ((name => +"Alice"));
        bob := borrower.create ((name => +"Bob"));
        carol := borrower.create ((name => +"Carol"));
        dave := borrower.create ((name => +"Dave"));

        glorious_deeds := book.create ((title => +"Glorious Deeds"));
        dark_doings := book.create ((title => +"Dark Doings"));
        mysterious_happenings := book.create
          ((title => +"Mysterious Happenings"));

        authorship.link (was_written_by => alice,
                         wrote => glorious_deeds);
        authorship.link (was_written_by =>
                         alice, wrote => dark_doings);
        authorship.link (was_written_by => bob,
                         wrote => mysterious_happenings);

        cl := current.link (is_on_loan_to => alice,
                            is_borrowing => mysterious_happenings);

        lh := history.link (has_been_loaned_to => alice,
                            has_borrowed => mysterious_happenings);

        lh := history.link (has_been_loaned_to => bob,
                            has_borrowed => glorious_deeds);
        lh := history.link (has_been_loaned_to => bob,
                            has_borrowed => dark_doings);

        lh := history.link (has_been_loaned_to => carol,
                            has_borrowed => mysterious_happenings);
        lh := history.link (has_been_loaned_to => carol,
                            has_borrowed => glorious_deeds);
        lh := history.link (has_been_loaned_to => carol,
                            has_borrowed => dark_doings);


***** (1) Authorship
define  b : borrower.handle;
        use type borrower.handle;
test    b := authorship.wrote (mysterious_happenings);
pass    b = bob

***** (2) Authorship
define  bks : book.collections.collection;
        use type book.handle;
test    bks := authorship.was_written_by (alice);
pass    book.collections.length (bks) = 2
        and  book.collections.location (bks, glorious_deeds) /= 0
        and  book.collections.location (bks, dark_doings) /= 0

***** (3) Authorship
define  bks : book.collections.collection;
        use type book.handle;
test    bks := authorship.was_written_by (carol);
pass    book.collections.length (bks) = 0

***** (4) Current Loan
define  b : borrower.handle;
        use type borrower.handle;
test    b := current.is_borrowing (glorious_deeds);
pass    b = null

***** (5) Current Loan
define  b : borrower.handle;
        use type borrower.handle;
test    b := current.is_borrowing (mysterious_happenings);
pass    b = alice

