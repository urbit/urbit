import React from "react";
import { useFormikContext } from "formik";
import { ErrorLabel } from "@tlon/indigo-react";

export function FormError(props: { message: string }) {
  const { status } = useFormikContext();

  let s = status || {};

  return (
    <ErrorLabel>{"error" in s ? props.message : null}</ErrorLabel>
  );
}
