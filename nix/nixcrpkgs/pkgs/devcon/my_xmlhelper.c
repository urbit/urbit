#include "xmlhelper.h"

EXTERN_C HRESULT InitXmlHelper()
{
    return 0;
}

EXTERN_C HRESULT ReleaseXmlWriter()
{
    return 0;
}

EXTERN_C HRESULT SaveXml(LPTSTR szfileName, DWORD dwCreationDisposition)
{
    MessageBox(NULL,
        "Sorry, XML saving is not supported in this build.",
        "XML not supported",
        MB_OK | MB_ICONEXCLAMATION);
    return 0;
}

EXTERN_C HRESULT XmlAddHostController(
    PSTR hcName,
    PUSBHOSTCONTROLLERINFO hcInfo
    )
{
    return 0;
}

EXTERN_C HRESULT XmlAddRootHub(PSTR rhName, PUSBROOTHUBINFO rhInfo)
{
    return 0;
}

EXTERN_C HRESULT XmlAddExternalHub(PSTR ehName, PUSBEXTERNALHUBINFO ehInfo)
{
    return 0;
}

EXTERN_C HRESULT XmlAddUsbDevice(PSTR devName, PUSBDEVICEINFO deviceInfo)
{
    return 0;
}

EXTERN_C VOID XmlNotifyEndOfNodeList(PVOID pContext)
{
}
