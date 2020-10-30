import React, { useState, useEffect } from "react";

import { Button, LoadingSpinner } from "@tlon/indigo-react";

import { useFormikContext } from "formik";

export function AsyncButton({
  children,
  ...rest
}: Parameters<typeof Button>[0]) {
  const { isSubmitting, status, isValid } = useFormikContext();
  const [success, setSuccess] = useState<boolean | undefined>();

  useEffect(() => {
    const s = status || {};
    let done = false;
    if ("success" in s) {
      setSuccess(true);
      done = true;
    } else if ("error" in s) {
      setSuccess(false);
      done = true;
    }
    if (done) {
      setTimeout(() => {
        setSuccess(undefined);
      }, 1500);
    }
  }, [status]);

  return (
    <Button disabled={!isValid} type="submit" {...rest}>
      {isSubmitting ? (
        <LoadingSpinner
          foreground={rest.primary ? "white" : 'black'}
          background="gray"
        />
      ) : success === true ? (
        "Done"
      ) : success === false ? (
        "Errored"
      ) : (
        children
      )}
    </Button>
  );
}
