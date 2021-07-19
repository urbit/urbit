import {
    Box, Col,

    ErrorLabel, Label,
    Row,

    StatelessTextInput as Input
} from '@tlon/indigo-react';
import { useField } from 'formik';
import React, { FormEvent, useState, useEffect } from 'react';
import { hexToUx } from '~/logic/lib/util';
import { uxToHex } from '@urbit/api/dist';

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
  const [{ value, onBlur }, meta, { setValue, setTouched }] = useField(id);
  const [field, setField] = useState(uxToHex(value));

  useEffect(() => {
    const newValue = hexToUx(padHex(field));
    setValue(newValue);
    setTouched(true);
  }, [field]);

  const onChange = (e: FormEvent<HTMLInputElement>) => {
    const { value: newValue } = e.target as HTMLInputElement;
    setField(newValue);
  };
  const isValid = value.match(COLOR_REGEX);
  const hex = uxToHex(value);

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
          onBlur={onBlur}
          onChange={onChange}
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
          />
        </Box>
      </Row>
      <ErrorLabel mt={2} hasError={Boolean(meta.touched && meta.error)}>
        {meta.error}
      </ErrorLabel>
    </Box>
  );
}
