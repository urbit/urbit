import React from "react";
import {
  Box,
  Row,
  Label,
  Col,
  ManagedRadioButtonField as Radio,
  ManagedTextInputField as Input,
} from "@tlon/indigo-react";

import GlobalApi from "~/logic/api/global";
import { S3State } from "~/types";
import { ImageInput } from "~/views/components/ImageInput";
import {ColorInput} from "~/views/components/ColorInput";

export type BgType = "none" | "url" | "color";

export function BackgroundPicker({
  bgType,
  bgUrl,
  api,
  s3,
}: {
  bgType: BgType;
  bgUrl?: string;
  api: GlobalApi;
  s3: S3State;
}) {

  const rowSpace = { my: 0, alignItems: 'center' };
  const radioProps = { my: 4, mr: 4, name: 'bgType' };
  return (
    <Col>
      <Label mb="2">Landscape Background</Label>
      <Row flexWrap="wrap" {...rowSpace}>
        <Radio {...radioProps} label="Image" id="url" />
        {bgType === "url" && (
          <ImageInput
            ml="3"
            api={api}
            s3={s3}
            id="bgUrl"
            name="bgUrl"
            label="URL"
            url={bgUrl || ""}
          />
        )}
      </Row>
      <Row {...rowSpace}>
        <Radio label="Color" id="color" {...radioProps} />
        {bgType === "color" && (
          <ColorInput id="bgColor" label="Color" /> 
        )}
      </Row>
      <Radio label="None" id="none" {...radioProps} />
    </Col>
  );
}
