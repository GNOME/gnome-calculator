[CCode (cheader_filename = "langinfo.h", cprefix = "")]
public enum NLItem
{
    RADIXCHAR,
    THOUSEP
}

[CCode (cheader_filename = "langinfo.h")]
public unowned string nl_langinfo (NLItem item);
