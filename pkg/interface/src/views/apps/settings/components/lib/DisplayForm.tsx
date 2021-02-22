import React from "react";

import {
  Box,
  ManagedCheckboxField as Checkbox,
  Button,
  Col,
  Text,
  ManagedToggleSwitchField as Toggle,
} from "@tlon/indigo-react";
import { Formik, Form } from "formik";
import * as Yup from "yup";

import GlobalApi from "~/logic/api/global";
import { uxToHex } from "~/logic/lib/util";
import { S3State, BackgroundConfig } from "~/types";
import { BackgroundPicker, BgType } from "./BackgroundPicker";
import { BackButton } from "./BackButton";
import useSettingsState, { SettingsState, selectSettingsState } from "~/logic/state/settings";

const formSchema = Yup.object().shape({
  bgType: Yup.string()
    .oneOf(["none", "color", "url"], "invalid")
    .required("Required"),
  background: Yup.string(),

});

interface FormSchema {
  bgType: BgType;
  bgColor: string | undefined;
  bgUrl: string | undefined;
}

interface DisplayFormProps {
  api: GlobalApi;
  s3: S3State;
}

const settingsSel = selectSettingsState(["display"]);

export default function DisplayForm(props: DisplayFormProps) {
  const { api, s3 } = props;

  const {
    display: {
      background,
      backgroundType,
    }
  } = useSettingsState(settingsSel);

  console.log(backgroundType);

  let bgColor, bgUrl;
  if (backgroundType === "url") {
    bgUrl = background; 
  }
  if (backgroundType === "color") {
    bgColor = background;
  }
  const bgType = backgroundType || "none";

  return (
    <Formik
      validationSchema={formSchema}
      initialValues={
        {
          bgType: backgroundType,
          bgColor: bgColor || "",
          bgUrl
        } as FormSchema
      }
      onSubmit={async (values, actions) => {
        let promises = [] as Promise<any>[];
        promises.push(api.settings.putEntry('display', 'backgroundType', values.bgType));

        promises.push(
          api.settings.putEntry('display', 'background', 
            values.bgType === "color"
            ? `#${uxToHex(values.bgColor || "0x0")}`
            : values.bgType === "url"
            ? values.bgUrl || "" 
            : false
          ));

        await Promise.all(promises);

      }}
    >
      {(props) => (
        <Form>
          <Col p="5" gapY="5">
            <BackButton />
            <Col gapY="2">
              <Text color="black" fontSize={2} fontWeight="medium">
                Display Preferences
              </Text>
              <Text gray fontSize="0">
                Customize visual interfaces across your Landscape
              </Text>
            </Col>
            <BackgroundPicker
              bgType={props.values.bgType}
              bgUrl={props.values.bgUrl}
              api={api}
              s3={s3}
            />
            <Button primary width="fit-content" type="submit">
              Save
            </Button>
          </Col>
        </Form>
      )}
    </Formik>
  );
}
