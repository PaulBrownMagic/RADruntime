% Todos
:- object(todos,
    imports([fluentc, actorc])).

   action(add_todo(_)).
   action(remove_todo(_)).
   action(mark_complete(_)).

   :- public(current_todo/2).
   current_todo(todo(Label, Status), do(A, S)) :-
       ( A = add_todo(Label), Status = todo
       ; A = mark_complete(Label), Status = complete
       ; current_todo(todo(Label, Status), S), A \= remove_todo(Label), A \= mark_complete(Label)
       ).

:- end_object.

% Actions
:- object(add_todo(_Label_),
    extends(action)).

   poss(S) :-
       \+ todos::current_todo(todo(_Label_, todo), S).

:- end_object.


:- object(remove_todo(_Label_),
    extends(action)).

   poss(S) :-
       todos::current_todo(todo(_Label_, _), S).

:- end_object.


:- object(mark_complete(_Label_),
    extends(action)).

   poss(S) :-
       todos::current_todo(todo(_Label_, todo), S).

:- end_object.

% GUI
:- object(xpce).
   :- include('xpce_includes.lgt').

   :- public(init/0).

   :- public(id/1).
   id(@ID) :-
       self(Self),
       functor(Self, ID, _).

   :- public(object/0).
   object :-
       ::id(ID),
       xobject(ID).

   :- public(object/1).
   object(O) :-
       xobject(O).

   :- public(size/2).
   size(W, H) :-
       ::get(size, size(W, H)).

   :- public(new/1).
   new(O) :-
       ::id(ID),
       xnew(ID, O).

   :- public(get/2).
   get(P, O) :-
       ::id(ID),
       xget(ID, P, O).

   :- public(selected_key/1).
   selected_key(O) :-
       ::get(selection, S),
       xget(S, key, O).

   :- public(send/1).
   send(M) :-
       ::id(ID),
       xsend(ID, M).

   :- public(send/2).
   send(P, O) :-
       ::id(ID),
       xsend(ID, P, O).

   :- public(send/3).
   send(P, O, M) :-
       ::id(ID),
       xsend(ID, P, O, M).

   :- public(free/0).
   free :-
       ::id(ID),
       xfree(ID).

:- end_object.


:- object(window,
    extends(xpce)).

   init :-
       ^^new(frame('ToDo')),
       ^^send(open).

   :- public(append/1).
   append(O) :-
       O::id(ID),
       ^^send(append, ID).

   :- public(append/3).
   append(O, Dir, Ref) :-
       O::id(OID),
       ^^send(append, OID),
       Ref::id(Rid),
       Where =.. [Dir, Rid],
       ^^send(OID, Where).

:- end_object.


:- object(todo_dialog,
    extends(xpce)).
    btn(button(new_todo, logtalk(new_todo_dialog, init))).
    btn(button(mark_complete, logtalk(app, mark_complete))).
    btn(button(remove_todo, logtalk(app, remove_todo))).

    init :-
        ^^new(dialog),
        ^^send(append(text('ToDos'))),
        forall(btn(B), ^^send(append(B))),
        self(Self),
        window::append(Self),
        ^^send(layout_dialog).

:- end_object.


:- object(todo_browser,
    extends(xpce)).

   init :-
       ^^new(browser),
       self(Self),
       window::append(Self),
       ^^send(below(@todo_dialog)).

   :- public(update/1).
   update(Labels) :-
      ^^send(members(Labels)).

:- end_object.


:- object(completed_dialog,
    extends(xpce)).
    btn(button(remove_completed, logtalk(app, remove_completed))).

    init :-
        ^^new(dialog),
        ^^send(append(text('Completed'))),
        forall(btn(B), ^^send(append(B))),
        self(Self),
        window::append(Self),
        ^^send(layout_dialog),
        ^^send(below(@todo_browser)).


:- end_object.


:- object(completed_browser,
    extends(xpce)).

   init :-
       ^^new(browser),
       self(Self),
       window::append(Self),
       ^^send(below(@completed_dialog)).

   :- public(update/1).
   update(Labels) :-
       ^^send(members(Labels)).

:- end_object.


:- object(app_dialog,
    extends(xpce)).
    btn(button(exit, logtalk(app, close))).

    init :-
        ^^new(dialog),
        forall(btn(B), ^^send(append(B))),
        self(Self),
        window::append(Self),
        ^^send(below(@completed_browser)).

:- end_object.


:- object(todo_name,
    extends(xpce)).
   init :-
       ^^new(text_item(new_todo_name)).
:- end_object.


:- object(new_todo_dialog,
    extends(xpce)).
    btn(button(save, logtalk(new_todo_dialog, save))).
    btn(button(cancel, logtalk(new_todo_dialog, close))).

    init :-
        ^^new(dialog('New Project')),
        todo_name::init,
        ::append(todo_name),
        forall(btn(B), ^^send(append(B))),
        ^^send(open).

    :- public(close/0).
    close :-
        todo_name::free,
        ::free.

    :- public(save/0).
    save :-
        todo_name::get(selection, ToDoName),
        todos::do(add_todo(ToDoName)),
        ::close.

   :- public(append/1).
   append(O) :-
       O::id(ID),
       ^^send(append, ID).

:- end_object.

% View
:- object(xpce_view,
    imports(view)).

    :- uses(logtalk, [
            print_message/3
        ]).

    render(Sit) :-
        findall(ToDo, situation::holds(todos::current_todo(ToDo), Sit), ToDos),
        print_message(information, rad, 'ToDos'::ToDos),
        findall(Action, situation::poss(Action, Sit), Actions),
        print_message(information, rad, 'Actions'::Actions),
        findall(TL,
           todos::holds(current_todo(todo(TL, todo))),
           ToDoLabels),
        todo_browser::update(ToDoLabels),
        findall(CL,
           todos::holds(current_todo(todo(CL, complete))),
           CompletedLabels),
        completed_browser::update(CompletedLabels).


:- end_object.

% App
:- object(app).

    :- public(init/0).
    init :-
        sm::init_sit(s0),
        window::init,
        todo_dialog::init,
        todo_browser::init,
        completed_dialog::init,
        completed_browser::init,
        app_dialog::init,
        !.

    :- public(close/0).
    close :-
        window::free.

    :- public(mark_complete/0).
    mark_complete :-
        todo_browser::selected_key(TD),
        todos::do(mark_complete(TD)).

    :- public(remove_todo/0).
    remove_todo :-
        todo_browser::selected_key(TD),
        todos::do(remove_todo(TD)).

    :- public(remove_completed/0).
    remove_completed :-
        completed_browser::selected_key(TD),
        todos::do(remove_todo(TD)).

:- end_object.
