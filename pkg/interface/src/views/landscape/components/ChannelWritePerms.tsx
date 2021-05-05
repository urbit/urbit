import {
  Col, Label,

  ManagedRadioButtonField as Radio
} from '@tlon/indigo-react';
import { useFormikContext } from 'formik';
import React from 'react';
import { ShipSearch } from '~/views/components/ShipSearch';

export type WritePerms = 'everyone' | 'subset' | 'self';
export interface ChannelWriteFieldSchema {
  writePerms: WritePerms;
  writers: string[];
}

interface ChannelWritePermsProps {
}

export function ChannelWritePerms<
  T extends ChannelWriteFieldSchema = ChannelWriteFieldSchema
>(props: ChannelWritePermsProps) {
  const { values, errors } = useFormikContext<T>();

  return (
    <Col gapY="3">
      <Label> Write Access</Label>
      <Radio name="writePerms" id="everyone" label="All group members" />
      <Radio name="writePerms" id="self" label="Only host" />
      <Radio name="writePerms" id="subset" label="Host and selected ships" />
      {values.writePerms === 'subset' && (
        <ShipSearch
          id="writers"
          label=""
          maxLength={undefined}
        />
      )}
    </Col>
  );
}
