import React from "react";
import { useField } from "formik";
import styled from "styled-components";
import { Col, InputLabel, Row, Box, ErrorMessage } from "@tlon/indigo-react";

import { uxToHex, hexToUx } from "~/logic/lib/util";

const Input = styled.input`
  background-color: ${ p => p.theme.colors.white };
  color: ${ p => p.theme.colors.black };
  box-sizing: border-box;
  border: 1px solid;
  border-right: none;
  border-color: ${(p) => p.theme.colors.lightGray};
  border-top-left-radius: ${(p) => p.theme.radii[2]}px;
  border-bottom-left-radius: ${(p) => p.theme.radii[2]}px;
  padding: ${(p) => p.theme.space[2]}px;
  font-size: 12px;
  line-height: 1.2;
`;

type ColorInputProps = Parameters<typeof Col>[0] & {
  id: string;
  label: string;
}

export function ColorInput(props: ColorInputProps) {
  const { id, label, ...rest } = props;
  const [{ value }, { error }, { setValue }] = useField(id);

  const hex = value.substr(2).replace('.', '');
  const padded = hex.padStart(6, '0');

  const onChange = (e: any) => {
    const { value: newValue } = e.target as HTMLInputElement;
    const valid = newValue.match(/^(\d|[a-f]|[A-F]){0,6}$/);

    if(!valid) {
      return;
    }
    const result = hexToUx(newValue);
    setValue(result);
  };

  return (
    <Col {...rest}>
      <InputLabel htmlFor={id}>{label}</InputLabel>
      <Row mt={2}>
        <Input onChange={onChange} value={hex} />
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
      <ErrorMessage mt="2">{error}</ErrorMessage>
    </Col>
  );
}
