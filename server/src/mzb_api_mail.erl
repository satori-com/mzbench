-module(mzb_api_mail).

-export([send/5, format_email/5]).

% Example:
% sendmail(<<"sender@example.com">>, <<"receiver@example.com">>, <<"Photos">>,
%          <<"See photo in attachment">>,
%          [{ImgName, "image/jpeg", ImgBin}])

send(To, Subject, Body, Attachments, Config) ->
    From = proplists:get_value(from, Config, <<"mz-bench-api@localhost">>),
    {FromAddr, ToAddr, EncodedBody} = format_email(From, To, Subject, Body, Attachments),
    SmtpOptions = proplists:get_value(smtp, Config),
    smtp_or_sendmail(FromAddr, ToAddr, EncodedBody, SmtpOptions).

smtp_or_sendmail(_, ToAddr, EncodedBody, undefined) ->
    _ = exec_with_input(
        "sendmail ~s", [ToAddr], [stderr_to_stdout],
        EncodedBody, mzb_api_app:default_logger());
smtp_or_sendmail(FromAddr, ToAddr, EncodedBody, SmtpOptions) ->
  <<"Ok", _/binary>> = gen_smtp_client:send_blocking({FromAddr, [ToAddr], EncodedBody}, SmtpOptions).

format_email(From, To, Subject, Body, Attachments) ->
    MimeBody = {<<"text">>, <<"plain">>,
                [{<<"Content-Type">>, <<"text/plain;charset=utf-8">>},
                 {<<"Content-Transfer-Encoding">>, <<"quoted-printable">>},
                 {<<"Content-Disposition">>, <<"inline">>}],
                [],
                Body},
    MimeAttachments = [attachment(A) || A <- Attachments],
    Mimemail = {<<"multipart">>,
               <<"mixed">>,
               [{<<"From">>, From},
                {<<"To">>, To},
                {<<"Subject">>, Subject}],
               [],
               [MimeBody | MimeAttachments]},
    FromAddr = extract_addr_rfc822(From),
    {FromAddr, extract_addr_rfc822(To), mimemail:encode(Mimemail)}.

attachment({Name, MimeType, Body}) ->
   [Ct1, Ct2] = binary:split(MimeType, <<"/">>),
   {Ct1, Ct2,
    [{<<"Content-Transfer-Encoding">>, <<"base64">>}],
    [{<<"disposition">>, <<"attachment">>},
     {<<"disposition-params">>,
      [{<<"filename">>, Name}]}],
    Body}.

extract_addr_rfc822(Rfc822) ->
    {ok, [{_, Addr}]} = smtp_util:parse_rfc822_addresses(Rfc822),
    list_to_binary(Addr).

exec_with_input(Format, Args, Opts, Input, Logger) ->
    File = mzb_file:tmp_filename(),
    ok = file:write_file(File, Input),
    try
        % Ports don't send eof to a programm but 'cat' does it
        mzb_subprocess:exec_format("cat ~s | " ++ Format, [File | Args], Opts, Logger)
    after
        ok = file:delete(File)
    end.

