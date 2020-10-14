import React from "react";
import { useFormikContext } from "formik";
import { ErrorMessage } from "@tlon/indigo-react";

export function FormError(props: { message: string }) {
  const { status } = useFormikContext();

  let s = status || {};

  return (
    <ErrorMessage>{"error" in s ? props.message : null}</ErrorMessage>
  );
}
