import _ from 'lodash';
import {
    Box, Col,
    ErrorLabel, Label,
    Row,
    StatelessTextInput as Input
} from '@tlon/indigo-react';
import { useField } from 'formik';
import React, { useState, useEffect, ChangeEvent, useMemo } from 'react';
import { uxToHex, hexToUx } from '@urbit/api';
import styled from 'styled-components';

export type ColorInputProps = Parameters<typeof Col>[0] & {
  id: string;
  label?: string;
  placeholder?: string;
  disabled?: boolean;
};

const COLOR_REGEX = /^(\d|[a-f]|[A-F]){6}$/;

function isValidHex(color: string): boolean {
  return COLOR_REGEX.test(color);
}

function parseIncomingColor(value: string): string {
  if (!value)
    return '';

  const isUx = value.startsWith('0x');
  return isUx ? uxToHex(value) : value.replace('#', '');
}

const ClickInput = styled(Input)`
  cursor: pointer;
`;

export function ColorInput(props: ColorInputProps) {
  const { id, placeholder, label, caption, disabled, ...rest } = props;
  const [{ value }, meta, { setValue, setTouched }] = useField(id);
  const [field, setField] = useState(parseIncomingColor(value));

  const update = (value: string) => {
    const normalizedValue = value.trim().replace(/[^a-f\d]/gi, '').slice(0,6);
    setField(normalizedValue);
  };

  const onText = (e: ChangeEvent<HTMLInputElement>) => update(e.target.value);

  const pickerChange = useMemo(() => _.debounce(update, 300), []);

  const updateField = useMemo(() => _.debounce((field: string) => {
    const newValue = hexToUx(field);
    setValue(newValue);
    setTouched(true);
  }, 100), []);

  useEffect(() => {
    if (isValidHex(field)) {
      updateField(field);
    }
  }, [field]);

  useEffect(() => {
    const parsedColor = parseIncomingColor(value);

    if (parsedColor !== field) {
      update(parsedColor);
    }
  }, [value]);

  const isValid = isValidHex(field);

  return (
    <Box display='flex' flexDirection='column' {...rest}>
      <Label htmlFor={id}>{label}</Label>
      {caption ? (
        <Label mt={2} gray>
          {caption}
        </Label>
      ) : null}
      <Row mt={2} alignItems='flex-end' maxWidth="120px">
        <Input
          id={id}
          borderTopRightRadius={0}
          borderBottomRightRadius={0}
          onBlur={onText}
          onChange={onText}
          value={field}
          disabled={disabled || false}
          borderRight={0}
          placeholder={placeholder}
        />
        <Box
          borderBottomRightRadius={1}
          borderTopRightRadius={1}
          border={1}
          borderColor='lightGray'
          width='32px'
          alignSelf='stretch'
          bg={isValid ? `#${field}` : 'transparent'}
        >
          <ClickInput
            width='100%'
            height='100%'
            alignSelf='stretch'
            disabled={disabled || false}
            type='color'
            value={`#${isValid ? field : uxToHex(value)}`}
            opacity={0}
            overflow='hidden'
            onChange={(e: ChangeEvent<HTMLInputElement>) => pickerChange(e.target.value)}
          />
        </Box>
      </Row>
      <ErrorLabel mt={2} hasError={Boolean(meta.touched && meta.error)}>
        {meta.error}
      </ErrorLabel>
    </Box>
  );
}
