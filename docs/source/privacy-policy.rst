Eta Privacy Policy
==================

We track data about users of the Eta website, the Eta compiler, the Etlas build tool,
and any other services we may offer from time to time.

This page tells you what we track, and what we do with it.

The Eta Compiler
----------------

All invocations of the Eta compiler store metrics, but the compiler itself does not
send these metrics to an external server. That function is performed by the Etlas build
tool, detailed in the following section.

For example, this is a sample payload of metrics stored by the Eta compiler:

{"startTime": "2017-07-20T20:02:00.164452Z","mode": 0,"endTime": "2017-07-20T20:02:08.721724Z"}

This includes:

1. The start time of the compiler invocation>
2. The mode of compilation, e.g., one shot or batch.
3. The end time of the compiler invocation.

The Etlas Build Tool
--------------------

The Etlas build tool does not track metrics of its own and sends the metrics
stored by the Eta compiler along with a uniquely identifier for the user's system
to the Eta telemetry server, only if the user had decided to opt-in - a preference
stored in the user's `Etlas config file <http://eta-lang.org/docs/html/faq.html#opt-in-telemetry>`_. All the requests to the telemetry server are logged and retained by
TypeLead, Inc.

This is a sample of the request log stored on the telemetry server, sent by the Etlas
build tool:

91.230.47.3 - - [24/07/2017:02:25:07 +0000] "GET /metrics-api HTTP/1.1" 200 - "" "etlas/1.0.1.0 (osx; x86_64)"

This includes:

1. The IP address of the requestor.
2. The date and time of the request.
3. The HTTP request URL and method.
4. The HTTP protocol version.
5. The HTTP response status code.
6. The length (in bytes) of the response that is sent back.
7. A Referrer header.
8. The User Agent header, which includes the Etlas version.

The Etlas build tool also queries a binary package server and decides whether to
install a given package in binary form or compile from source. All the requests to the
binary package server are logged and retained by TypeLead, Inc.

This is a sample of the request log stored on the binary package server, sent by the
Etlas build tool:

91.230.47.3 - - [20/07/2017:15:45:35 +0000] "GET /index HTTP/1.1" 304 290 "-" "etlas/1.0.1.0 (osx; x86_64)"

This includes the same details as listed above, since the format is exactly the same.

The Eta Website
---------------

Like most website operators, TypeLead, Inc. collects non-personally-identifying
information of the sort that web browsers and servers typically make available, such as
the browser type, language preference, referring site, and the date and time of each
visitor request. TypeLead, Inc.'s purpose in collecting non-personally identifying
information is to better understand how the visitors of the Eta website use the
website. From time to time, TypeLead, Inc. may release non-personally-identifying
information in the aggregate, e.g., by publishing a report on trends in website usage.

TypeLead, Inc. also collects potentially personally-identifying information like
Internet Protocol (IP) addresses. TypeLead, Inc. does not use such information to
identify its visitors, however, and does not disclose such information, other than
under the same circumstances that it uses and discloses personally-identifying
information, as described below.

The Eta website uses Google Analytics to monitor and analyze user behavior. This
service provides TypeLead, Inc. with information on users' demographics, age, location,
and interest categories, when such information is available. This information is not
used to identify individual users, but can in some cases be very specific. You can
learn more about the information gathered and retained by this service at the `Google
Analytics privacy policy <https://support.google.com/analytics/answer/2700409?hl=en&utm_id=ad>`_.
You can opt out of Google Analytics entirely with the `Google Analytics opt-out browser addon <https://support.google.com/analytics/answer/181881?hl=en&ref_topic=2919631>`_.

Collection of Personally Identifying Information
------------------------------------------------

In order to write information into the Eta telemetry server, users may decide to
provide certain personally identifying information including but not limited to: email
address, username, password, personal website, and account names on other services such
as GitHub, Twitter, and IRC.

If users do not want their information tracked in this manner, they can opt out by
following the instructions `here <http://eta-lang.org/docs/html/faq.html#opt-out-telemetry>`_. However, this means that some features of the Eta website will be unavailable
to them.

Use of Personally Identifying Information
-----------------------------------------

We may use personally identifying information we have collected about you, including
your email address, to provide you with news, notes, and recommendations. You can opt
out of receiving such messages at any time by using the "unsubscribe" links or
directions at the ends of messages you receive. In addition, we use collected
personally identifying information to operate our business and the Eta service. We do
not disclose your personal information to unaffiliated third parties who may want to
offer you their own products and services unless you have requested or authorized us
to do so.

We may share your personal information with third parties or affiliates where it is
necessary for us to complete a transaction or do something you have asked us to do.
Likewise, we may share your personal information with third parties or affiliates with whom we have contracted to perform services on our behalf. Companies that act on our
behalf are required to keep the personal information we provide to them confidential
and to use the personal information we share only to provide the services we ask them
to perform.

In addition, we may disclose personal information in the good faith belief that we are
lawfully authorized to do so, or that doing so is reasonably necessary to comply with
legal process or authorities, respond to any claims, or to protect the rights, property
, or personal safety of TypeLead Inc., our users, our employees, or the public. In
addition, information about our users, including personal information, may be
disclosed or transferred as part of, or during negotiations of, any merger, sale of
company assets, or acquisition.

Cookies
-------

A cookie is a string of information that a website stores on a visitor's computer, and that the visitor's browser provides to the website each time the visitor returns.

The Eta website uses cookies to help identify and track visitors, their usage of the
Eta website, and their website access credentials. Eta website visitors who do not wish
to have cookies placed on their computers should set their browsers to refuse cookies
before using TypeLead, Inc.'s websites, with the drawback that certain features of
TypeLead, Inc.'s websites may not function properly without the aid of cookies.

Disclosure of Log Information
-----------------------------

All user information is retained in raw form for such time as deemed appropriate by
TypeLead, Inc. It is shared with employees and contractors of TypeLead, Inc., as
needed to process information on TypeLead, Inc.'s behalf.

Raw log data is not shared with third parties, but may be shared in aggregate. For
example, we may share the number of active Eta users in a given day, and occasionally
TypeLead, Inc. may publish blog posts or reports on the Eta compiler or Eta website
usage.

We also analyze log data for a variety of reasons, including counting up downloads and unique visitors, debugging production problems, tracking which versions of Eta and
Etlas are in use in the wild, and researching how Eta packages are used together with
one another. This helps us to better understand the usage patterns of Eta, and make
better decisions about the evolving Eta in the future.
 
Use by Minors
-------------

We will refer to the Eta website, Etlas build tool (when opted-in for telemetry), and
other related services as the "Eta services." Eta services are not intended for
use by minor children (under the age of 18). Parents and guardians should monitor the
use of the Eta service by minor children. Children under age 13 should not use Eta
services at all. If a child under age 13 submits personal information through any part
of the service, and we become aware that the person submitting the information is under
age 13, we will attempt to delete the information as soon as reasonably possible.

Links to Other Websites
-----------------------

The Eta website may contain links to other websites. Any personal information you
provide on the linked pages is provided directly to that third party and is subject to
that third party's privacy policy. Except as described above, we are not responsible
for the content or privacy and security practices and policies of websites to which we
link. Links from the Eta service to third parties or to other sites are provided for
your convenience. We encourage you to learn about their privacy and security practices
and policies before providing them with personal information.

United States Jurisdiction
--------------------------

The Eta website and related services are hosted in the United States. This Privacy
Policy is intended to comply with privacy laws in the United States and may not
comply with all privacy laws in other countries.

If you are a non-US user of the service, by using our service and providing us with
data, you acknowledge, agree and provide your consent that your personal information
may be processed in the United States for the purposes identified in this Privacy
Policy. In addition, such data may be stored on servers located outside your resident
jurisdiction, which may have less stringent privacy practices than your own. By using
the supporting Eta services and providing us with your data, you consent to the
transfer of such data and any less stringent privacy practices.

Contact Information
-------------------

If you have any questions or concerns about how we track user information, or how that
information is used, please contact us at once.

You may contact TypeLead, Inc. by emailing `legal@typelead.com <mailto:legal@typelead.com>`_.

Privacy Policy Changes
----------------------

Although most changes are likely to be minor, TypeLead, Inc. may change its Privacy
Policy from time to time, and in TypeLead, Inc.'s sole discretion. Any such changes
will be posted on `the Eta blog <https://medium.com/eta-programming-language>`_, and
the detailed history of changes can be found in `the git repository history for this document <https://github.com/typelead/eta/blob/master/docs/source/privacy-policy.rst>`_.

TypeLead, Inc. encourages visitors to frequently check this page for any changes to its
Privacy Policy. Your continued use of the Eta website, the Etlas build tool (if
already opted-in), and any supporting services after any change in this Privacy Policy
will constitute your acceptance of such change.

Credit and License
------------------

Parts of this policy document were originally included in the `npm Privacy Policy <https://www.npmjs.com/policies/privacy>`_.

This document may be reused under a `Creative Commons Attribution-ShareAlike License <https://creativecommons.org/licenses/by-sa/4.0/>`_.
