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

export type ColorInputProps = Parameters<typeof Col>[0] & {
  id: string;
  label?: string;
  placeholder?: string;
  disabled?: boolean;
};

const COLOR_REGEX = /^(\d|[a-f]|[A-F]){6}$/;

function padHex(hex: string) {
  if(hex.length === 0) {
    return '000000';
  }
  const repeat = 6 / hex.length;
  if(Math.floor(repeat) === repeat) {
    return hex.repeat(repeat);
  }
  if(hex.length < 6) {
    return hex.slice(0,3).repeat(2);
  }
  return hex.slice(0,6);
}

export function ColorInput(props: ColorInputProps) {
  const { id, placeholder, label, caption, disabled, ...rest } = props;
  const [{ value }, meta, { setValue, setTouched }] = useField(id);
  const [field, setField] = useState(uxToHex(value));

  const update = (value: string) => {
    const normalizedValue = value.trim().replace(/[^a-f\d]/gi, '').slice(0,6);
    setField(normalizedValue);
  };

  const onText = (e: ChangeEvent<HTMLInputElement>) => update(e.target.value);

  const pickerChange = useMemo(() => _.debounce(update, 100), []);

  const updateField = useMemo(() => _.debounce((field: string) => {
    const newValue = hexToUx(padHex(field));
    console.log({ field, newValue });
    setValue(newValue);
    setTouched(true);
  }, 150), []);

  useEffect(() => {
    updateField(field);
  }, [field]);

  const hex = uxToHex(value);
  const isValid = COLOR_REGEX.test(hex);

  return (
    <Box display='flex' flexDirection='column' {...rest}>
      <Label htmlFor={id}>{label}</Label>
      {caption ? (
        <Label mt={2} gray>
          {caption}
        </Label>
      ) : null}
      <Row mt={2} alignItems='flex-end'>
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
          borderLeft={0}
          borderColor='lightGray'
          width='32px'
          alignSelf='stretch'
          bg={isValid ? `#${hex}` : 'transparent'}
        >
          <Input
            width='100%'
            height='100%'
            alignSelf='stretch'
            disabled={disabled || false}
            type='color'
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
