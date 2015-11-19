%%% @doc
%%% This module is the primary interface GUI programmers will deal with. It contains
%%% wrappers for wxErlang commands to build, display and interact with meta-widgets
%%% built from wxErlang components while (hopefully) leaving the calling Erlang code
%%% in as idiomatic condition as possible. The goal is to reduce the vast amount of
%%% not-really-C++ boilerplate code necessary to write Erlang GUI applications.
%%%
%%% Widgets constructed by ZXW do not use wxErlang stock buttons because of the
%%% paradigm clashes inherent in mixing WX-style localization techniques and Erlang
%%% functional code. Image buttons are used instead to sidestep the issue of language
%%% entirely, and this approach lends itself to employment of an interface look and
%%% feel somewhere between common web-widgets and Android/tablet style buttons modern
%%% users are growing more accustomed to. As a side-effect of this decision, however,
%%% GUI applications that use zx_widgets with image buttons will never adhere to any
%%% given system's "native look-and-feel". On the other hand, a given zx_widgets
%%% project will maintain a common look and feel across platforms.
%%%
%%% == Image file names ==
%%% Image file names are defined in macros and always indicate PNG files. Image formats
%%% are restricted to PNG to avoid inconsistent auto-detection problems on various
%%% platforms. Currently required image files are:
%%% <ul>
%%%   <li>zxw_button_add.png</li>
%%%   <li>zxw_button_del.png</li>
%%%   <li>zxw_button_affirm.png</li>
%%%   <li>zxw_button_cancel.png</li>
%%% </ul>
%%% Stock images are included in the themes/standard/icons/ directory of this project.
%%% @end

-module(zxw).
-include_lib("wx/include/wx.hrl").
-export([flags/1,
         text_input_grid/3, list_picker/7, list_control/4, modal_text_input/4,
         yes_no_box/1, png_button/3,
         show_message/2]).


-define(iconADD,    "zxw_button_add.png").
-define(iconDEL,    "zxw_button_del.png").
-define(iconAFFIRM, "zxw_button_affirm.png").
-define(iconCANCEL, "zxw_button_cancel.png").

-type tag()            :: atom().
-type label()          :: unicode:chardata().
-type rank()           :: {tag(), label()}.
-type field_index()    :: blank
                        | {tag(), label}
                        | {label, tag()}
                        | {tag(), tag()}.
-type indexed_widget() :: {field_index(), wx:wx_object()}.

%% @doc
%% Returns sane expansion flags (proportion = 0) for use in
%% `wxSizer:add(This, Window, Options)'.
%% Literally defined as:
%%  ```
%%  flags(base) -> [{proportion, 0}, {flag, ?wxEXPAND}];
%%  flags(wide) -> [{proportion, 1}, {flag, ?wxEXPAND}].
%%  '''
-spec flags(Style) -> [Flag]
    when Style     :: base | wide,
         Flag      :: {proportion, Size} | {flags, WxExpand},
         Size      :: 0 | 1,
         WxExpand  :: integer().
flags(base) -> [{proportion, 0}, {flag, ?wxEXPAND}];
flags(wide) -> [{proportion, 1}, {flag, ?wxEXPAND}].

%% @doc
%% Creates a "Yes/No" or "Affirm/Cancel" button box with a "Yes/Affirm" button on
%% the left and a "No/Cancel" button on the right. The return value is a triple
%% of the form `{AffirmButton, CancelButton, ButtonSizer}' to allow the calling code
%% to set focus or manipulate aspects of the elements returned as desired.
%% @end
-spec yes_no_box(WxParent) -> {AffirmB, CancelB, ButtonBox}
    when WxParent  :: wx:wx_object(),
         AffirmB   :: wx:wx_object(),
         CancelB   :: wx:wx_object(),
         ButtonBox :: wx:wx_object().
yes_no_box(WxParent) ->
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    {ok, IconDir} = zxw_control:get_conf(icon_dir),
    AffirmB = png_button(WxParent, ?wxID_OK, filename:join(IconDir, ?iconAFFIRM)),
    CancelB = png_button(WxParent, ?wxID_OK, filename:join(IconDir, ?iconCANCEL)),
    _ = wxSizer:add(Sizer, AffirmB),
    _ = wxSizer:add(Sizer, CancelB),
    {AffirmB, CancelB, Sizer}.

%% @doc
%% Creates a wxBitmapButton using a PNG image file. The image file name passed to
%% this function must be resolvable by the executing node -- any calls to assist
%% in path construction (such as `zxw_control:get_conf(icon_dir)') must happen
%% before calling this function.
-spec png_button(WxParent, ID, ImageFilePath) -> Button
    when WxParent      :: wx:wx_object(),
         ID            :: integer(),
         ImageFilePath :: string(),
         Button        :: wx:wx_object().
png_button(WxParent, ID, ImageFilePath) ->
    Icon = wxBitmap:new(ImageFilePath, [{type, ?wxBITMAP_TYPE_PNG}]),
    wxBitmapButton:new(WxParent, ID, Icon).

%% @doc
%% Displays a notification modal to the user. Accepts and displays arbitrary Erlang
%% terms. If the Message input is printable unicode then the message will be displayed
%% with string `"~ts"' formatting, otherwise Erlang term `"~tp"' formatting will be
%% used. Always returns `ok', regardless how the modal is closed.
-spec show_message(wx:wx_object(), unicode:chardata() | term()) -> ok.
show_message(WxParent, Message) ->
    Format = case io_lib:printable_unicode_list(Message) of
        true  -> "~ts";
        false -> "~tp"
    end,
    Modal = wxMessageDialog:new(WxParent, io_lib:format(Format, [Message])),
    _ = wxMessageDialog:showModal(Modal),
    ok = wxMessageDialog:destroy(Modal).

%% @doc
%% Accepts a title, subtitle (header), list of labels, and a parent window,
%% and creates a modal input window on the screen of the general form:
%%   ```
%%   Label1: Input1
%%   Label2: Input2
%%   ...
%%   '''
%%
%% Depends on `yes_no_box/1', meaning it has textless buttons that reference
%% the PNG image paths `?iconAFFIRM' and `?iconCANCEL'.
%%
%% Returns an `ok'-tuple containing utf8 strings ordered according to the
%% order of the initial Labels argument or the atom `cancel'.
-spec modal_text_input(WxParent, Title, Header, Labels) -> Result
    when WxParent  :: wx:wx_object(),
         Title     :: unicode:chardata(),
         Header    :: unicode:chardata(),
         Labels    :: [unicode:chardata()],
         Result    :: {ok, Values} | cancel,
         Values    :: [unicode:chardata()].
modal_text_input(WxParent, Title, Header, Labels) ->
    Dialog = wxDialog:new(WxParent, ?wxID_ANY, Title),
    Sz = wxBoxSizer:new(?wxVERTICAL),
    HeadSz = wxStaticBoxSizer:new(?wxVERTICAL, Dialog, [{label, Header}]),
    GridSz = wxFlexGridSizer:new(length(Labels), 2, 4, 4),
    {_, _, ButtSz} = yes_no_box(Dialog),

    ok = wxFlexGridSizer:setFlexibleDirection(GridSz, ?wxHORIZONTAL),
    ok = wxFlexGridSizer:addGrowableCol(GridSz, 1),
    ok = wxStaticBoxSizer:setMinSize(HeadSz, 300, 70),
    _ = wxSizer:add(Sz, HeadSz, flags(wide)),
    _ = wxSizer:add(Sz, ButtSz, [{flag, ?wxCENTER}]),
    _ = wxSizer:add(HeadSz, GridSz, flags(wide)),

    MakeInputElement =
        fun(Label) ->
            L = wxStaticText:new(Dialog, ?wxID_ANY, Label),
            T = wxTextCtrl:new(Dialog, ?wxID_ANY, [{style, ?wxTAB_TRAVERSAL}]),
            _ = wxSizer:add(GridSz, L, flags(base)),
            _ = wxSizer:add(GridSz, T, flags(wide)),
            T
        end,
    Elements = lists:map(MakeInputElement, Labels),

    ok = wxDialog:setSizer(Dialog, Sz),
    ok = wxSizer:layout(Sz),
    ok = wxDialog:setSize(Dialog, wxDialog:getBestSize(Dialog)),
    ok = wxTextCtrl:setFocus(hd(Elements)),

    case wxDialog:showModal(Dialog) of
        ?wxID_OK     -> {ok, [wxTextCtrl:getValue(E) || E <- Elements]};
        ?wxID_CANCEL -> cancel
    end.

%% @doc
%% Creates a grid of text input fields with a row of text headers along the top and
%% column of text labels along the left side. Given the following code:
%%  ```
%%  Cols = [{family, "Family"}, {given, "Given"}],
%%  Rows = [{romaji, "Romaji"}, {kanji, "Kanji"}],
%%  {Grid, FieldList} = text_input_grid(Cols, Rows, WxFrame)
%%  '''
%% `Grid' will  be a wxFlexGridSizer wx:wx_object(), suitable for inclusion in a
%% sizer created in the calling code, and `FieldList' will be a list of input fields
%% contained in a list of the form `[{Index, wx:wx_object()}]', where `Index' is the
%% tuple `{ColTag, RowTag}'.
%%
%% The returned grid, when rendered, will appear as:
%%  ```
%%           Family     Given
%%  Romaji  [______]   [______]
%%  Kanji   [______]   [______]
%%  '''
-spec text_input_grid(WxParent, Rows, Cols) -> {GridSz, FieldList}
    when WxParent   :: wx:wx_object(),
         Rows       :: [rank()],
         Cols       :: [rank()],
         GridSz     :: wx:wx_object(),
         FieldList  :: [indexed_widget()].
text_input_grid(WxParent, Rows, Cols) ->
    Width = length(Cols) + 1,

    GridSz = wxFlexGridSizer:new(Width, [{vgap, 4}, {hgap, 4}]),
    ok = wxFlexGridSizer:setFlexibleDirection(GridSz, ?wxHORIZONTAL),
    Flexerize = fun(Col) -> wxFlexGridSizer:addGrowableCol(GridSz, Col) end,
    ok = lists:foreach(Flexerize, lists:seq(1, Width)),

    TopRow = render_head(WxParent, Cols),
    DataRows = render_body(WxParent, Rows, Cols),
    All = lists:flatten([TopRow | DataRows]),

    Add = fun ({_, Widget}) -> wxSizer:add(GridSz, Widget, flags(wide)) end,
    ok = lists:foreach(Add, All),

    FieldList = lists:flatten([tl(DataRow) || DataRow <- DataRows]),
    {GridSz, FieldList}.

-spec render_head(WxParent, Cols) -> Widgets
    when WxParent :: wx:wx_object(),
         Cols     :: [rank()],
         Widgets  :: [indexed_widget()].
render_head(WxParent, Cols) ->
    Spacer = {blank, wxBoxSizer:new(?wxHORIZONTAL)},
    Labels = render_head(WxParent, Cols, []),
    [Spacer | Labels].

-spec render_head(WxParent, Cols, Acc) -> Widgets
    when WxParent :: wx:wx_object(),
         Cols     :: [rank()],
         Acc      :: [indexed_widget()],
         Widgets  :: [indexed_widget()].
render_head(_, [], Acc) ->
    lists:reverse(Acc);
render_head(WxParent, [{Tag, Label} | Cols], Acc) ->
    Element = {{label, Tag}, wxStaticText:new(WxParent, ?wxID_ANY, Label)},
    render_head(WxParent, Cols, [Element | Acc]).

-spec render_body(WxParent, Rows, Cols) -> Widgets
    when WxParent :: wx:wx_object(),
         Rows     :: [rank()],
         Cols     :: [rank()],
         Widgets  :: [[indexed_widget()]].
render_body(WxParent, Rows, Cols) ->
    render_body(WxParent, Rows, Cols, []).

-spec render_body(WxParent, Rows, Cols, Acc) -> Widgets
    when WxParent :: wx:wx_object(),
         Rows     :: [rank()],
         Cols     :: [rank()],
         Acc      :: [indexed_widget()],
         Widgets  :: [[indexed_widget()]].
render_body(_, [], _, Acc) ->
    lists:reverse(Acc);
render_body(WxParent, [{Tag, Label} | Rows], Cols, Acc) ->
    First = {{Tag, label}, wxStaticText:new(WxParent, ?wxID_ANY, Label)},
    Fields = render_row(WxParent, Tag, Cols, []),
    render_body(WxParent, Rows, Cols, [[First | Fields] | Acc]).

-spec render_row(WxParent, Rows, Cols, Acc) -> Widgets
    when WxParent :: wx:wx_object(),
         Rows     :: [rank()],
         Cols     :: [rank()],
         Acc      :: [indexed_widget()],
         Widgets  :: [indexed_widget()].
render_row(_, _, [], Acc) ->
    lists:reverse(Acc);
render_row(WxParent, RTag, [{CTag, _} | Cols], Acc) ->
    InputField = wxTextCtrl:new(WxParent, ?wxID_ANY, [{style, ?wxTAB_TRAVERSAL}]),
    render_row(WxParent, RTag, Cols, [{{RTag, CTag}, InputField} | Acc]).

%% @doc
%% Creates a wxListCtrl, places it within a wxStaticBoxSizer and adds "add element" and
%% "del element" buttons. The object identities must be passed in to ensure that
%% listening for `#wx{}' event messages remains simple (matching on `#wx{id=ID}',
%% for example). The wxListCtrl will be populated with whatever elements are provided
%% in `Items'.
%%
%% Returns a 4-tuple of: `{WxListCtrl, AddB, DelB, Sizer}' to allow calling code
%% to perform any custom manipulations desired.
-spec list_picker(WxParent, PickerID, AddID, DelID, Headers, Items, Label) -> Result
    when WxParent   :: wx:wx_object(),
         PickerID   :: integer(),
         AddID      :: integer(),
         DelID      :: integer(),
         Headers    :: [{Label :: unicode:chardata(), PixWidth :: integer()}],
         Items      :: [[unicode:chardata()]],
         Label      :: unicode:chardata(),
         Result     :: {WxListCtrl :: wx:wx_object(),
                        AddB       :: wx:wx_object(),
                        DelB       :: wx:wx_object(),
                        Sizer      :: wx:wx_object()}.
list_picker(WxParent, PickerID, AddID, DelID, Headers, Items, Label) ->
    Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, WxParent, [{label, Label}]),
    Picker = list_control(WxParent, PickerID, Headers, Items),
    {ok, IconDir} = zxw_control:get_conf(icon_dir),
    AddButton = png_button(WxParent, AddID, filename:join(IconDir, ?iconADD)),
    DelButton = png_button(WxParent, DelID, filename:join(IconDir, ?iconDEL)),
    ButtSz = wxBoxSizer:new(?wxVERTICAL),
    _ = wxSizer:add(Sizer, Picker, flags(wide)),
    _ = wxSizer:add(ButtSz, AddButton, flags(base)),
    _ = wxSizer:add(ButtSz, DelButton, flags(base)),
    _ = wxSizer:add(Sizer, ButtSz, flags(base)),
    {Picker, AddButton, DelButton, Sizer}.

%% @doc
%% Creates a wxListCtrl and arranges the headers and any item inserts, and returns
%% the `wx:wx_object()' reference to the wxListCtrl.
-spec list_control(WxParent, PickerID, Headers, Items) -> Picker
    when WxParent   :: wx:wx_object(),
         PickerID   :: integer(),
         Headers    :: [{Label :: unicode:chardata(), PixWidth :: integer()}],
         Items      :: [[unicode:chardata()]],
         Picker     :: wx:wx_object().
list_control(WxParent, PickerID, Headers, Items) ->
    Picker = wxListCtrl:new(WxParent, [{winid, PickerID},
                                       {style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
    ColNums = lists:seq(0, length(Headers) - 1),
    Cols = lists:zip(ColNums, Headers),
    AddCol =
        fun({Col, {Header, Width}}) ->
            wxListCtrl:insertColumn(Picker, Col, Header, [{width, Width}])
        end,
    ok = lists:foreach(AddCol, Cols),
    AddRow =
        fun({Row, Atts}) ->
            wxListCtrl:insertItem(Picker, Row, ""),
            SetCell = fun({Col, Data}) -> wxListCtrl:setItem(Picker, Row, Col, Data) end,
            ok = lists:foreach(SetCell, lists:zip(ColNums, tuple_to_list(Atts)))
        end,
    ok = lists:foreach(AddRow, lists:zip(lists:seq(0, length(Items) -1), Items)),
    ok = wxListBox:connect(Picker, command_list_item_activated, [{skip, true}]),
    Picker.
