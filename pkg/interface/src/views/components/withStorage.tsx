import React from "react";
import useStorage from "~/logic/lib/useStorage";

const withStorage = (Component, params = {}) => {
  return React.forwardRef((props: any, ref) => {
    const storage = useStorage(props.s3, props.gcp, params);

    return <Component ref={ref} {...storage} {...props} />;
  });
};

export default withStorage;
