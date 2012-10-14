public const string VERSION;
public const string GETTEXT_PACKAGE;
public const string LOCALE_DIR;
public const string UI_DIR;

[CCode (cheader_filename = "langinfo.h", cprefix = "")]
public enum NLItem
{
    RADIXCHAR,
    THOUSEP
}

[CCode (cheader_filename = "langinfo.h")]
public unowned string nl_langinfo (NLItem item);
