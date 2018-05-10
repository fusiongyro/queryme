:- use_module(library(yall)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

:- use_module(library(md/md_parse)).

% we need this module from the HTTP client library for http_read_data
:- use_module(library(http/http_client)).
:- http_handler('/', list_posts, []).
:- http_handler('/', show_post, [prefix]).

:- dynamic post/1.

:- json_object post(id,uuid,title,slug,markdown,mobiledoc,html,image,featured,page,status,language,visibility,meta_title,meta_description,author_id,created_at,created_by,updated_at,updated_by,published_at,published_by).

save_post(post(_,_,Title,Slug,Markdown,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)) :-
    asserta(post(Title, Slug, Markdown)).

load_posts :-
    open("blog.json", read, Stream),
    json_read(Stream, json([db=[json(L)]])),
    retractall(post(_)),
    member(data=json(X), L),
    member(posts=Posts, X),
    maplist(json_to_prolog, Posts, FormattedPosts),
    forall(member(Post, FormattedPosts), save_post(Post)),
    close(Stream).

:- load_posts.


% Listing posts
list_posts(_Request) :-
    blog_titles(Titles),
    maplist([X,Y]>>(Y=li([],X)), Titles, Links),
    reply_html_page(title("Daniel's Blog"),
                    [ol([], Links)]).

blog_title(a([href=Slug], [Title])) :- post(Title, Slug, _).
blog_titles(Titles) :-  findall(Title, blog_title(Title), Titles).

% Showing posts
show_post(Request) :-
    member(path_info(Path), Request),
    post(Title,Path,Markdown),
    md_parse_string(Markdown, Body),
    reply_html_page(title(["Daniel's Blog - ", Title]), Body).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).

% :- server(8888).
