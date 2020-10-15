import React from "react";
import { useField } from "formik";
import styled from "styled-components";
import {
  Col,
  Label,
  Row,
  Box,
  ErrorLabel,
  StatelessTextInput as Input,
} from "@tlon/indigo-react";

import { uxToHex, hexToUx } from "~/logic/lib/util";

type ColorInputProps = Parameters<typeof Col>[0] & {
  id: string;
  label: string;
  disabled: boolean;
};

export function ColorInput(props: ColorInputProps) {
  const { id, label, caption, disabled, ...rest } = props;
  const [{ value, onBlur }, meta, { setValue }] = useField(id);

  const hex = value.substr(2).replace(".", "");
  const padded = hex.padStart(6, "0");

  const onChange = (e: any) => {
    const { value: newValue } = e.target as HTMLInputElement;
    const valid = newValue.match(/^(\d|[a-f]|[A-F]){0,6}$/);

    if (!valid) {
      return;
    }
    const result = hexToUx(newValue);
    setValue(result);
  };

  return (
    <Box display="flex" flexDirection="column" {...props}>
      <Label htmlFor={id}>{label}</Label>
      {caption ? (
        <Label mt="2" gray>
          {caption}
        </Label>
      ) : null}
      <Row mt="2" alignItems="flex-end">
        <Input
          borderTopRightRadius={0}
          borderBottomRightRadius={0}
          onBlur={onBlur}
          onChange={onChange}
          value={hex}
          disabled={disabled || false}
        />
        <Box
          borderBottomRightRadius={1}
          borderTopRightRadius={1}
          border={1}
          borderLeft={0}
          borderColor="lightGray"
          width="32px"
          alignSelf="stretch"
          bg={`#${padded}`}
        />
      </Row>
      <ErrorLabel mt="2" hasError={!!(meta.touched && meta.error)}>
        {meta.error}
      </ErrorLabel>
    </Box>
  );
}
