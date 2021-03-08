import React from "react";

import {
  Col,
  Text,
  Label,
  ManagedRadioButtonField as Radio
} from "@tlon/indigo-react";
import { Formik, Form } from "formik";
import * as Yup from "yup";

import GlobalApi from "~/logic/api/global";
import { uxToHex } from "~/logic/lib/util";
import { S3State, BackgroundConfig, StorageState } from "~/types";
import { BackgroundPicker, BgType } from "./BackgroundPicker";
import useSettingsState, { SettingsState, selectSettingsState } from "~/logic/state/settings";
import {AsyncButton} from "~/views/components/AsyncButton";
import { BackButton } from "./BackButton";

const formSchema = Yup.object().shape({
  bgType: Yup.string()
    .oneOf(["none", "color", "url"], "invalid")
    .required("Required"),
  background: Yup.string(),
  theme: Yup.string()
    .oneOf(["light", "dark", "auto"])
    .required("Required")
});

interface FormSchema {
  bgType: BgType;
  bgColor: string | undefined;
  bgUrl: string | undefined;
  theme: string;
}

interface DisplayFormProps {
  api: GlobalApi;
}

const settingsSel = selectSettingsState(["display"]);

export default function DisplayForm(props: DisplayFormProps) {
  const { api } = props;

  const {
    display: {
      background,
      backgroundType,
      theme
    }
  } = useSettingsState(settingsSel);


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
          bgUrl,
          theme
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

        promises.push(api.settings.putEntry('display', 'theme', values.theme));
        await Promise.all(promises);

        actions.setStatus({ success: null });

      }}
    >
      {(props) => (
        <Form>
          <BackButton/>
          <Col p="5" pt="4" gapY="5">
              <Col gapY="1" mt="0">
              <Text color="black" fontSize={2} fontWeight="medium">
                Display Preferences
              </Text>
              <Text gray>
                Customize visual interfaces across your Landscape
              </Text>
            </Col>
            <BackgroundPicker
              bgType={props.values.bgType}
              bgUrl={props.values.bgUrl}
              api={api}
            />
            <Label>Theme</Label>
            <Radio name="theme" id="light" label="Light"/>
            <Radio name="theme" id="dark" label="Dark" />
            <Radio name="theme" id="auto" label="Auto" />
            <AsyncButton primary width="fit-content" type="submit">
              Save
            </AsyncButton>
          </Col>
        </Form>
      )}
    </Formik>
  );
}
