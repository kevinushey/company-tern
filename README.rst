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

If you don't like circles after object's own properties consider less
annoying marker for that purpose.

.. code:: lisp

    (setq company-tern-own-property-marker "")

You can trim too long function signatures to the frame width.

.. code:: lisp

    (setq company-tern-meta-as-single-line t)

Known issues
------------

If annotations appear inline with corresponding identifiers and sans
any tabulation, then you need to set up the align option properly.

.. code:: lisp

    (setq company-tooltip-align-annotations t)

Thanks
------

* **@katspaugh**
* **@dgutov**

.. _Tern: http://ternjs.net/
.. _company-mode: http://company-mode.github.io/
.. _Melpa: http://melpa.milkbox.net/
