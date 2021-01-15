import React from "react";
import { useFormikContext } from "formik";
import { ErrorLabel } from "@tlon/indigo-react";
import {PropFunc} from "~/types/util";

export function FormError(props: { message: string } & PropFunc<typeof ErrorLabel>) {
  const { status } = useFormikContext();
  const { message, ...rest } = props;

  let s = status || {};

  return (
    <ErrorLabel {...rest} hasError={"error" in s}>{message}</ErrorLabel>
  );
}
