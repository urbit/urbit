import React from "react";

import {
  Box,
  Label,
  ManagedCheckboxField as Checkbox,
  Button,
} from "@tlon/indigo-react";
import { Formik, Form } from "formik";
import * as Yup from "yup";
import _ from "lodash";

import GlobalApi from "../../../../api/global";
import { LaunchState } from "../../../../types/launch-update";
import { DropLaunchTiles } from "./DropLaunch";
import { S3State, BackgroundConfig } from "../../../../types";
import { BackgroundPicker, BgType } from "./BackgroundPicker";

const formSchema = Yup.object().shape({
  tileOrdering: Yup.array().of(Yup.string()),
  bgType: Yup.string()
    .oneOf(["none", "color", "url"], "invalid")
    .required("Required"),
  bgUrl: Yup.string().url(),
  bgColor: Yup.string().matches(/#([A-F]|[a-f]|[0-9]){6}/, "Invalid color"),
  avatars: Yup.boolean(),
  nicknames: Yup.boolean(),
});

interface FormSchema {
  tileOrdering: string[];
  bgType: BgType;
  bgColor: string | undefined;
  bgUrl: string | undefined;
  avatars: boolean;
  nicknames: boolean;
}

interface DisplayFormProps {
  api: GlobalApi;
  launch: LaunchState;
  dark: boolean;
  background: BackgroundConfig;
  hideAvatars: boolean;
  hideNicknames: boolean;
  s3: S3State;
}

export default function DisplayForm(props: DisplayFormProps) {
  const { api, launch, background, hideAvatars, hideNicknames, s3 } = props;

  let bgColor, bgUrl;
  if (background?.type === "url") {
    bgUrl = background.url;
  }
  if (background?.type === "color") {
    bgColor = background.color;
  }
  const bgType = background?.type || "none";

  return (
    <Formik
      validationSchema={formSchema}
      initialValues={
        {
          bgType,
          bgColor,
          bgUrl,
          avatars: hideAvatars,
          nicknames: hideNicknames,
          tileOrdering: launch.tileOrdering,
        } as FormSchema
      }
      onSubmit={(values, actions) => {
        api.launch.changeOrder(values.tileOrdering);

        const bgConfig: BackgroundConfig =
          values.bgType === "color"
            ? { type: "color", color: values.bgColor || "" }
            : values.bgType === "url"
            ? { type: "url", url: values.bgUrl || "" }
            : undefined;

        api.local.setBackground(bgConfig);
        api.local.hideAvatars(values.avatars);
        api.local.hideNicknames(values.nicknames);
        api.local.dehydrate();
        actions.setSubmitting(false);
      }}
    >
      {(props) => (
        <Form>
          <Box
            display="grid"
            gridTemplateColumns="100%"
            gridTemplateRows="auto"
            gridRowGap={5}
          >
            <Box color="black" fontSize={1} mb={3} fontWeight={900}>
              Display Preferences
            </Box>
            <Box mb={2}>
              <Label display="block" pb={2}>
                Tile Order
              </Label>
              <DropLaunchTiles
                id="tileOrdering"
                name="tileOrdering"
                tiles={launch.tiles}
                order={launch.tileOrdering}
              />
            </Box>
            <BackgroundPicker
              bgType={props.values.bgType}
              bgUrl={props.values.bgUrl}
              api={api}
              s3={s3}
            />
            <Checkbox
              label="Disable avatars"
              id="avatars"
              caption="Do not show user-set avatars"
            />
            <Checkbox
              label="Disable nicknames"
              id="nicknames"
              caption="Do not show user-set nicknames"
            />
            <Button border={1} style={{ cursor: 'pointer' }} borderColor="washedGray" type="submit">
              Save
            </Button>
          </Box>
        </Form>
      )}
    </Formik>
  );
}
