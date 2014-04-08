Company tern
============

Tern_ backend for company-mode_.

Installation
------------

You can install this package from Melpa_::

    M-x package-install RET company-tern RET

Usage
-----

Add ``company-tern`` to allowed ``company-mode`` backends list

.. code:: lisp

    (add-to-list 'company-backends 'company-tern)

Knowing issues
--------------

If annotations appears inline with corresponding identifiers without
any tabulation then you must to set align option properly.

.. code:: lisp

    (setq company-tooltip-align-annotations t)

.. _Tern: http://ternjs.net/
.. _company-mode: http://company-mode.github.io/
.. _Melpa: http://melpa.milkbox.net/
