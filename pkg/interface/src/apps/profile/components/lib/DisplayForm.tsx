import React, { useCallback } from "react";

import {
  Input,
  Box,
  Center,
  Col,
  InputLabel,
  Radio,
  Checkbox,
  Button,
} from "@tlon/indigo-react";

import { Formik, Form } from "formik";
import * as Yup from "yup";
import _ from "lodash";
import GlobalApi from "../../../../api/global";
import { BackgroundConfig } from "../../../../types/local-update";
import { LaunchState } from "../../../../types/launch-update";
import { DropLaunchTiles } from "./DropLaunch";

const tiles = ["publish", "links", "chat", "dojo", "clock", "weather"];

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

type BgType = "none" | "url" | "color";

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
}

function ImagePicker({ url }: { url: string }) {
  return (
    <Center
      width="250px"
      height="250px"
      p={3}
      backgroundImage={`url('${url}')`}
      backgroundSize="cover"
    >
      <Box>Change</Box>
    </Center>
  );
}

function BackgroundPicker({
  bgType,
  bgUrl,
}: {
  bgType: BgType;
  bgUrl?: string;
}) {
  return (
    <Box>
      <InputLabel>Landscape Background</InputLabel>
      <Box display="flex" alignItems="center">
        <Box mt={3} mr={10}>
          <Radio label="Image" id="url" name="bgType" />
          <Radio label="Color" id="color" name="bgType" />
          <Radio label="None" id="none" name="bgType" />
        </Box>
        {bgType === "url" && (
          <Col>
            <Input ml={4} type="text" label="URL" id="bgUrl" name="bgUrl" />
          </Col>
        )}
        {bgType === "color" && (
          <Input ml={4} type="text" label="Color" id="bgColor" name="bgColor" />
        )}
      </Box>
    </Box>
  );
}

export default function DisplayForm(props: DisplayFormProps) {
  const { api, launch, background, hideAvatars, hideNicknames } = props;

  let bgColor, bgUrl;
  if (background?.type === "url") {
    bgUrl = background.url;
  }
  if (background?.type === "color") {
    bgColor = background.color;
  }
  const bgType = background?.type || "none";

  const logoutAll = useCallback(() => {}, []);

  console.log(tiles);

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
        console.log("saving");
        console.log(values.tileOrdering);
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
            gridTemplateColumns="1fr"
            gridTemplateRows="auto"
            gridRowGap={3}
          >
            <Box color="black" fontSize={1} mb={3} fontWeight={900}>
              Display Preferences
            </Box>

            <Box mb={2}>
              {/*<Input
                label="Home Tile Order"
                id="order"
                type="text"
                width={256}
              />*/}
              <InputLabel display="block" pb={2}>
                Tile Order
              </InputLabel>
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
            />
            <Box>
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
            </Box>
          </Box>
          <Button border={1} borderColor="washedGray" type="submit">
            Save
          </Button>
        </Form>
      )}
    </Formik>
  );
}
