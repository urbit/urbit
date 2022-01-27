export const postReactNativeMessage = (data: any) => {
  if ((window as any).ReactNativeWebView?.postMessage) {
    (window as any).ReactNativeWebView.postMessage(JSON.stringify(data));
  }
};
