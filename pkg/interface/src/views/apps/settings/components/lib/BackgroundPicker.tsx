import {
  Col, Label,

  ManagedRadioButtonField as Radio, Row, Text
} from '@tlon/indigo-react';
import React, { ReactElement } from 'react';
import { ColorInput } from '~/views/components/ColorInput';
import { ImageInput } from '~/views/components/ImageInput';

export type BgType = 'none' | 'url' | 'color';

export function BackgroundPicker(): ReactElement {
  const rowSpace = { my: 0, alignItems: 'center' };
  const colProps = {
    my: 3,
    mr: 4,
    gapY: 1,
    minWidth: '266px',
    width: ['100%', '288px']
  };
  return (
    <Col>
      <Label>Groups Background</Label>
      <Row flexWrap="wrap" {...rowSpace}>
        <Col {...colProps}>
          <Radio mb={1} name="bgType" label="Image" id="url" />
          <Text ml={5} gray>Set an image background</Text>
          <ImageInput
            ml={5}
            id="bgUrl"
            placeholder="Drop or upload a file, or paste an image URL here"
            name="bgUrl"
          />
        </Col>
      </Row>
      <Row {...rowSpace}>
        <Col {...colProps}>
          <Radio mb={1} label="Color" id="color" name="bgType" />
          <Text ml={5} gray>Set a hex-based background</Text>
          <ColorInput placeholder="FFFFFF" ml={5} id="bgColor" />
        </Col>
      </Row>
      <Radio
        my={3}
        caption="Your home screen will simply render as its respective day/night mode color"
        name="bgType"
        label="None"
        id="none"
      />
    </Col>
  );
}
