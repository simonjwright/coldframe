-- $Id: library-test.ts,v 95067db7e369 2001/05/06 18:02:04 simon $

-- A TG test script to check ColdFrame's navigation of Associations.

error_handling
        continue

context
        with Ada.Text_IO; use Ada.Text_IO;
	with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
        with ColdFrame.Exceptions;

        with library.book.all_instances;
        with library.book.collections;
        with library.borrower.all_instances;
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
        glorious_deeds, dark_doings, fiendish_frolics, mysterious_happenings :
           book.handle;
        cl : current_loan.handle;
        lh : loan_history.handle;

code
        alice := borrower.create ((name => +"Alice"));
        bob := borrower.create ((name => +"Bob"));
        carol := borrower.create ((name => +"Carol"));
        dave := borrower.create ((name => +"Dave"));

        glorious_deeds := book.create
           ((title => +"Glorious Deeds"));
        dark_doings := book.create
           ((title => +"Dark Doings"));
        fiendish_frolics := book.create
           ((title => +"Fiendish Frolics"));
        mysterious_happenings := book.create
           ((title => +"Mysterious Happenings"));

        authorship.link (was_written_by => alice,
                         wrote => glorious_deeds);
        authorship.link (was_written_by => alice,
                         wrote => dark_doings);
        authorship.link (was_written_by => alice,
                         wrote => fiendish_frolics);
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


***** (1) 1:m
define  b : borrower.handle;
        use type borrower.handle;
test    b := authorship.wrote (null);    -- gives constraint_error. hmm.
pass    b = null

***** (2) 1:m
define  b : borrower.handle;
        use type borrower.handle;
test    b := authorship.wrote (mysterious_happenings);
pass    b = bob

***** (3) 1:m
define  bks : book.collections.collection;
        use type book.handle;
test    bks := authorship.was_written_by (null);
pass    book.collections.length (bks) = 0

***** (4) 1:m
define  bks : book.collections.collection;
        use type book.handle;
test    bks := authorship.was_written_by (carol);
pass    book.collections.length (bks) = 0

***** (5) 1:m
define  bks : book.collections.collection;
        use type book.handle;
test    bks := authorship.was_written_by (alice);
pass    book.collections.length (bks) = 3
        and  book.collections.location (bks, glorious_deeds) /= 0
        and  book.collections.location (bks, dark_doings) /= 0
        and  book.collections.location (bks, fiendish_frolics) /= 0
        and  book.collections.location (bks, mysterious_happenings) = 0

***** (6) 1:m collection
define  bs : borrower.collections.collection;
test    bs := authorship.wrote (book.collections.null_container);
pass    borrower.collections.length (bs) = 0

***** (7) 1:m collection
define  bs : borrower.collections.collection;
test    bs := authorship.wrote (book.all_instances);
pass    borrower.collections.length (bs) = 2
        and borrower.collections.location (bs, alice) /= 0
        and borrower.collections.location (bs, bob) /= 0
        and borrower.collections.location (bs, carol) = 0
        and borrower.collections.location (bs, dave) = 0

***** (8) 1:m
define  bks : book.collections.collection;
test    bks := authorship.was_written_by (borrower.collections.null_container);
pass    book.collections.length (bks) = 0

***** (9) 1:m
define  bks : book.collections.collection;
test    bks := authorship.was_written_by (borrower.all_instances);
pass    book.collections.length (bks) = 4

***** (10) 1-(1:m)
define  cl : current_loan.handle;
test    cl := current.link (is_on_loan_to => alice,
                            is_borrowing => mysterious_happenings);
pass    exception coldframe.exceptions.duplicate;

***** (11) 1-(1:m)
define  b : borrower.handle;
        use type borrower.handle;
test    b := current.is_borrowing (book.handle'(null));
pass    b = null

***** (12) 1-(1:m)
define  b : borrower.handle;
        use type borrower.handle;
test    b := current.is_borrowing (glorious_deeds);
pass    b = null

***** (13) 1-(1:m)
define  b : borrower.handle;
        use type borrower.handle;
test    b := current.is_borrowing (mysterious_happenings);
pass    b = alice

***** (14) 1-(1:m)
define  bks : book.collections.collection;
test    bks := current.is_on_loan_to (null);
pass    book.collections.length (bks) = 0

***** (15) 1-(1:m)
define  bks : book.collections.collection;
test    bks := current.is_on_loan_to (dave);
pass    book.collections.length (bks) = 0

***** (16) 1-(1:m)
define  bks : book.collections.collection;
test    bks := current.is_on_loan_to (alice);
pass    book.collections.length (bks) = 1
        and book.collections.location (bks, glorious_deeds) = 0
        and book.collections.location (bks, dark_doings) = 0
        and book.collections.location (bks, fiendish_frolics) = 0
        and book.collections.location (bks, mysterious_happenings) /= 0

***** (17) 1-(m:m)
define  lh : loan_history.handle;
test    lh := history.link (has_been_loaned_to => alice,
                            has_borrowed => mysterious_happenings);
pass    exception coldframe.exceptions.duplicate;

***** (18) 1-(m:m)
define  bks : book.collections.collection;
test    bks := history.has_been_loaned_to (null);
pass    book.collections.length (bks) = 0

***** (19) 1-(m:m)
define  bks : book.collections.collection;
test    bks := history.has_been_loaned_to (dave);
pass    book.collections.length (bks) = 0

***** (20) 1-(m:m)
define  bks : book.collections.collection;
test    bks := history.has_been_loaned_to (bob);
pass    book.collections.length (bks) = 2
        and  book.collections.location (bks, glorious_deeds) /= 0
        and  book.collections.location (bks, dark_doings) /= 0
        and  book.collections.location (bks, fiendish_frolics) = 0
        and  book.collections.location (bks, mysterious_happenings) = 0

***** (21) 1-(m:m)
define  bs : borrower.collections.collection;
test    bs := history.has_borrowed (mysterious_happenings);
pass    borrower.collections.length (bs) = 2
        and borrower.collections.location (bs, alice) /= 0
        and borrower.collections.location (bs, carol) /= 0
        and borrower.collections.location (bs, bob) = 0
        and borrower.collections.location (bs, dave) = 0

***** (22) 1-(m:m) collection
define  bks : book.collections.collection;
test    bks := history.has_been_loaned_to (borrower.collections.null_container);
pass    book.collections.length (bks) = 0

***** (23) 1-(m:m) collection
define  bks : book.collections.collection;
test    bks := history.has_been_loaned_to (borrower.all_instances);
pass    book.collections.length (bks) = 3
        and  book.collections.location (bks, glorious_deeds) /= 0
        and  book.collections.location (bks, dark_doings) /= 0
        and  book.collections.location (bks, fiendish_frolics) = 0
        and  book.collections.location (bks, mysterious_happenings) /= 0

***** (24) 1-(m:m) collection
define  bs : borrower.collections.collection;
test    bs := history.has_borrowed (book.collections.null_container);
pass    borrower.collections.length (bs) = 0

***** (25) 1-(m:m) collection
define  bs : borrower.collections.collection;
test    bs := history.has_borrowed (book.all_instances);
pass    borrower.collections.length (bs) = 3
        and  borrower.collections.location (bs, alice) /= 0
        and  borrower.collections.location (bs, bob) /= 0
        and  borrower.collections.location (bs, carol) /= 0
        and  borrower.collections.location (bs, dave) = 0
