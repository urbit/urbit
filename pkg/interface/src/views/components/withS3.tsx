import React from "react";
import useS3 from "~/logic/lib/useS3";

const withS3 = (Component, params = {}) => {
  return React.forwardRef((props: any, ref) => {
    const s3 = useS3(props.s3, params);

    return <Component ref={ref} {...s3} {...props} />;
  });
};

export default withS3;