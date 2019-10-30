:- object(persistency,
    implements(monitoring)).

   :- info([ version is 1.0
           , author is 'Paul Brown'
           , date is 2019/10/4
           , comment is 'Persist instances of objects to a file'
           ]).

   :- private(file/1).
   file('todo_storage.pl').

   after(sm, do(_), _Sender) :-
       sm::sit(S),
       ::persist(S).

   :- public(persist/1).
   :- mode(persist(+term), zero_or_one).
   :- info(persist/1,
       [ comment is 'Persist the term to the file provided'
       , argnames is ['Term']
       ]).
   persist(Term) :-
       nonvar(Term),
       ::file(File),
       setup_call_cleanup(open(File, write, Stream), (writeq(Stream, Term), write(Stream, '.\n')), close(Stream)), !.

   :- public(restore/1).
   :- mode(restore(-term), one).
   :- info(restore/1,
       [ comment is 'Restore all persisted terms.'
       , argnames is ['Term']
       ]).
   restore(Term) :- var(Term),
       read_terms(Term),
       sm::init_sit(Term).

   % read in a list of repr Terms from persistent file
   read_terms(Terms) :-
       ::file(File), os::file_exists(File),
       setup_call_cleanup(open(File, read, Stream), read(Stream, Terms), close(Stream)).
   read_terms(ET) :-
       \+ (::file(File), os::file_exists(File)),
       situation::empty(ET).

:- end_object.
