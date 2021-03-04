import React, { ReactElement } from 'react';

import {
  Row,
  Label,
  Col,
  ManagedRadioButtonField as Radio,
} from '@tlon/indigo-react';

import GlobalApi from '~/logic/api/global';
import { ImageInput } from '~/views/components/ImageInput';
import { ColorInput } from '~/views/components/ColorInput';
import { StorageState } from '~/types';

export type BgType = 'none' | 'url' | 'color';

export function BackgroundPicker({
  bgType,
  bgUrl,
  api,
  storage
}: {
  bgType: BgType;
  bgUrl?: string;
  api: GlobalApi;
  storage: StorageState;
}) {
  const rowSpace = { my: 0, alignItems: 'center' };
  const radioProps = { my: 4, mr: 4, name: 'bgType' };
  return (
    <Col>
      <Label mb="2">Landscape Background</Label>
      <Row flexWrap="wrap" {...rowSpace}>
        <Radio {...radioProps} label="Image" id="url" />
        {bgType === 'url' && (
          <ImageInput
            ml="3"
            api={api}
            storage={storage}
            id="bgUrl"
            name="bgUrl"
            label="URL"
            url={bgUrl || ''}
          />
        )}
      </Row>
      <Row {...rowSpace}>
        <Radio label="Color" id="color" {...radioProps} />
        {bgType === 'color' && (
          <ColorInput id="bgColor" label="Color" />
        )}
      </Row>
      <Radio label="None" id="none" {...radioProps} />
    </Col>
  );
}
