import React, { ReactNode, useState, useEffect } from "react";

import { Button } from "@tlon/indigo-react";

import { Spinner } from "./Spinner";
import { useFormikContext } from "formik";

interface AsyncButtonProps {
  loadingText: string;
  children: ReactNode;
}
export function AsyncButton({
  loadingText,
  children,
  ...rest
}: AsyncButtonProps & Parameters<typeof Button>[0]) {
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
    <Button border disabled={!isValid} type="submit" {...rest}>
      {isSubmitting ? (
        <Spinner awaiting text={loadingText} />
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
