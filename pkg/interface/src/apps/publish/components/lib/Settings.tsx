import React, { Component, useEffect } from "react";
import { Spinner } from "../../../../components/Spinner";
import { AsyncButton } from "../../../../components/AsyncButton";
import { InviteSearch } from "../../../../components/InviteSearch";
import Toggle from "../../../../components/toggle";
import {
  Box,
  Input,
  Checkbox,
  Col,
  InputLabel,
  InputCaption,
  Button,
} from "@tlon/indigo-react";
import { Formik, Form, useFormikContext, FormikHelpers } from "formik";
import GlobalApi from "../../../../api/global";
import { Notebook } from "../../../../types/publish-update";
import { Contacts } from "../../../../types/contact-update";

/*
 *
 *
  renderGroupify() {
    const { props, state } = this;

    const owner = (props.host.slice(1) === window.ship);

    const ownedUnmanaged =
      owner &&
      !props.contacts[props.notebook?.['writers-group-path']];

    if (!ownedUnmanaged) {
      return null;
    } else {
      // don't give the option to make inclusive if we don't own the target
      // group
      const targetOwned = (state.targetGroup)
        ? Boolean(state.targetGroup.includes(`/~${window.ship}/`))
        : false;
      let inclusiveToggle = <div />;
      if (targetOwned) {
        inclusiveToggle = (
          <div className="mt4">
          <Toggle
            boolean={state.inclusive}
            change={this.changeInclusive}
          />
            <span className="dib f9 white-d inter ml3">
              Add all members to group
            </span>
            <p className="f9 gray2 pt1" style={{ paddingLeft: 40 }}>
              Add notebook members to the group if they aren't in it yet
            </p>
          </div>
        );
      }

      return (
        <div>
          <div className={'w-100 fl mt3 mb3'} style={{ maxWidth: '29rem' }}>
            {this.renderHeader(
              'Convert Notebook',
              'Convert this notebook into a group with associated chat, or select a group to add this notebook to.')}
            <InviteSearch
              groups={props.groups}
              contacts={props.contacts}
              associations={props.associations}
              groupResults={true}
              shipResults={false}
              invites={{
                groups: state.targetGroup ? [state.targetGroup] : [],
                ships: []
              }}
              setInvite={this.changeTargetGroup}
            />
            {inclusiveToggle}
            <button
               onClick={this.groupifyNotebook.bind(this)}
               className={'dib f9 black gray4-d bg-gray0-d ba pa2 mt4 b--black b--gray1-d pointer'}
               disabled={this.state.disabled}
            >
              {state.targetGroup ? 'Add to group' : 'Convert to group'}
            </button>
          </div>
        </div>
      );

{this.renderHeader(
            'Delete Notebook',
            'Permanently delete this notebook. (All current members will no longer see this notebook)')}
{() => {
                this.setState({ disabled: true });
                this.props.api.publish
                  .publishAction({
                    'edit-book': {
                      book: this.props.book,
                      title: this.state.title,
                      about: this.props.notebook.about,
                      coms: this.props.notebook.comments,
                      group: null
                    }
                  })
                  .then(() => {
                    this.setState({ disabled: false });
                  });
              }}
 */

interface SettingsProps {
  host: string;
  book: string;
  notebook: Notebook;
  contacts: Contacts;
  api: GlobalApi;
}

interface FormSchema {
  name: string;
  description: string;
  comments: boolean;
}

const ResetOnPropsChange = (props: { init: FormSchema; book: string }) => {
  const { resetForm } = useFormikContext<FormSchema>();
  useEffect(() => {
    resetForm({ values: props.init });
  }, [props.book]);

  return null;
};

export function Settings(props: SettingsProps) {
  const { host, notebook, api, book } = props;
  const initialValues: FormSchema = {
    name: notebook?.title,
    description: notebook?.about,
    comments: true,
  };

  const onSubmit = async (
    values: FormSchema,
    actions: FormikHelpers<FormSchema>
  ) => {
    await api.publish.publishAction({
      "edit-book": {
        book,
        title: values.name,
        about: values.description,
        coms: values.comments,
        group: null,
      },
    });
    api.publish.fetchNotebook(host, book)
  };

  if (props.host.slice(1) !== window.ship) {
    return null;
  }
  return (
    <Formik initialValues={initialValues} onSubmit={onSubmit}>
        <Form>
          <Box maxWidth="300px" mb={4} gridTemplateColumns="1fr" gridAutoRows="auto" display="grid">
            <Col mb={4}>
              <InputLabel>Delete Notebook</InputLabel>
              <InputCaption>
                Permanently delete this notebook. (All current members will no
                longer see this notebook
              </InputCaption>

              <Button mt={1} border error>
                Delete this notebook
              </Button>
            </Col>
            <Input
              id="name"
              label="Rename"
              caption="Change the name of this notebook"
            />
            <Input
              id="description"
              label="Change description"
              caption="Change the description of this notebook"
            />
            <Checkbox
              id="comments"
              label="Comments"
              caption="Subscribers may comment when enabled"
            />
            <ResetOnPropsChange init={initialValues} book={book} />
            <AsyncButton loadingText="Updating.." border>
              Save
            </AsyncButton>
          </Box>
        </Form>
    </Formik>
  );
}

export default Settings;
