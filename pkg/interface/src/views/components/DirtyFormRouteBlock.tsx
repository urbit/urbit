import React, { useCallback } from "react";
import { useFormikContext } from "formik";
import { Prompt } from "react-router-dom";

export function DirtyFormRouteBlock<T extends object = any>(props: {
  message?: string;
}) {
  const {
    message = "Are you sure you want to leave? You have unsaved changes",
  } = props;
  const { dirty, setFieldError, touched } = useFormikContext<T>();
  const onMessage = useCallback(() => {
    Object.keys(touched).map((field) => {
      if (touched[field]) {
        setFieldError(field, "Unsaved");
      }
    });
    return message;
  }, [setFieldError, message, touched]);

  return <Prompt message={onMessage} when={dirty} />;
}
