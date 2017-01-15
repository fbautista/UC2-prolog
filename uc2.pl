:- module(upload2, [ run/0]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_mime_plugin)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).

:- use_module(library(xpath)).

:-use_module(library(http/json)).
:-use_module(library(http/http_json)).

:- http_handler(root(.),	upload_form, []).
:- http_handler(root(upload),	upload,      []).

%:- use_module(library(semweb/rdf_db)).


%model(XML,ID,NAME)
model(XML,ID,NAME):-
  xpath(XML, //packagedElement(@'xmi:type'='uml:Model'),A),
  xpath(A, /self(@'xmi:id'),ID),
  xpath(A, /self(@'name'),NAME).

%package(XML,ID,NAME)
package(XML,ID,NAME):-
  xpath(XML, //packagedElement(@'xmi:type'='uml:Package'),A),
  xpath(A, /self(@'xmi:id'),ID),
  xpath(A, /self(@'name'),NAME).

%useCase(XML,ID, NAME)
useCase(XML,ID,NAME):-
  xpath(XML, //packagedElement(@'xmi:type'='uml:UseCase'),A),
  xpath(A, /self(@'xmi:id'),ID),
  xpath(A, /self(@'name'),NAME).

%actor(XML,ID,NAME)
actor(XML,ID,NAME):-
  xpath(XML, //packagedElement(@'xmi:type'='uml:Actor'),A),
  xpath(A, /self(@'xmi:id'),ID),
  xpath(A, /self(@'name'),NAME).


%association(XML, ArrowSource, ArrowTarget)
association(XML,Source,Target):-
  xpath(XML, //packagedElement(@'xmi:type'='uml:Association'),A),
  xpath(A, ownedEnd(1),O),
  xpath(O, /self(@'type'),Source),
  xpath(A, ownedEnd(2),U),
  xpath(U, /self(@'type'),Target).

%extend(XML, ArrowSource, ArrowTarget)
extend(XML,Source,Target):-
  xpath(XML, //packagedElement,A),
  xpath(A, /self(@'xmi:id'),Source),
  xpath(A, extend,E),
  xpath(E, /self(@'extendedCase'),Target).

%include(XML, ArrowSource, ArrowTarget)
include(XML,Source,Target):-
  xpath(XML, //packagedElement,A),
  xpath(A, /self(@'xmi:id'),Source),
  xpath(A, include,E),
  xpath(E, /self(@'addition'),Target).

%association(XML, ChildElement, ParentElement)
in(XML,Child,Parent):-
  xpath(XML, //packagedElement,A),
  xpath(A, /self(@'xmi:id'),Parent),
  xpath(A, packagedElement,E),
  xpath(E, /self(@'xmi:id'),Child).

%%Test Unrelated Actor%% <--
%unrelatedActor(XML, IdActor, NameActor)
unrelatedActor(XML,ID,NAME):- actor(XML,ID,NAME), not(association(XML,ID,_)).

%%Test UC accesibles%%
%accesible(XML, IdElement)
accessible(XML, X):- association(XML,A,X), actor(XML,A,_), useCase(XML,X,_). 
accessible(XML,X):- include(XML,Y,X), accessible(XML,Y).
accessible(XML,X):- extend(XML,X,Y), accessible(XML,Y).

%%Test UC isolated%% <--
%isolated(XML, IdUseCase, NameUseCase)
isolated(XML, X, N):-useCase(XML,X,N), not(accessible(XML,X)).

%%Test Actor Inside System%% <--
%actorInsideSystem(XML, IdActor, NameActor)
actorInsideSystem(XML,X,N):- actor(XML,X,N), package(XML ,B,_), in(XML ,X,B).
actorInsideSystem(XML,X,N):- actor(XML,X,N), model(XML ,B,_), in(XML ,X,B).

%%Test UC Inside Boundaries%%
%useCaseInsideBoundaries(XML, IdUseCase)
useCaseInsideBoundaries(XML,X):- useCase(XML,X,_), package(XML,B,_), in(XML,X,B).
useCaseInsideBoundaries(XML,X):- useCase(XML,X,_), model(XML,B,_), in(XML,X,B).

%%Test UC Outside Boundaries%% <--
%useCaseOutsideBoundaries(XML, IdUseCase, NameUseCase)
useCaseOutsideBoundaries (XML,X,N):- useCase(XML,X,N), not(useCaseInsideBoundarie(XML,X)).


%%Run server%%
run :-
        http_server(http_dispatch, [port(8080)]).


%%Upload Form%%
upload_form(_Request) :-
        reply_html_page(
            title('Upload a file'),
            [ h1('Upload a file'),
              form([ method('POST'),
                     action(location_by_id(upload)),
                     enctype('multipart/form-data')
                   ],
                   table([],
                         [ tr([td(input([type(file), name(file)]))]),
                           tr([td(align(right),
                                  input([type(submit), value('Upload!')]))])
                         ]))
            ]).


upload(Request) :-
        (   memberchk(method(post), Request),
            http_read_data(Request, Parts, [form_data(mime)]),
            member(mime(Attributes, Data, []), Parts),
            memberchk(name(file), Attributes),
            memberchk(filename(Target), Attributes)            
        ->  % process file here;
            load_xml(Target, XML, []),
            findall(F, isolated(XML,_,F), Isolated),
            findall(F, useCaseOutsideBoundaries(XML,_,F), UseCaseOutsideBoundaries),
            findall(F, actorInsideSystem(XML,_,F), actorInsideSystem),
            findall(F, unrelatedActor(XML,_,F), unrelatedActor),
            reply_json(json([ test=json([name='Test Isolated', message=Isolated]),
                              test=json([name='Test UseCaseOutsideBoundaries', message=UseCaseOutsideBoundaries]),
                              test=json([name='Test actorInsideSystem', message=actorInsideSystem]),
                              test=json([name='Test unrelatedActor', message=unrelatedActor])
            ])


              )
        ;   throw(http_reply(bad_request(bad_file_upload)))
        ).


:- multifile prolog:message//1.

prolog:message(bad_file_upload) -->
        [ 'A file upload must be submitted as multipart/form-data using', nl,
          'name=file and providing a file-name'
        ].
