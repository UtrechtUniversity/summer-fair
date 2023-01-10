import logging
import uuid
import zipfile
import os
import azure.functions as func
from fastapi import FastAPI, Request, Depends
from fastapi.responses import FileResponse

from pathlib import Path

from azure.identity import DefaultAzureCredential
from azure.mgmt.resource import ResourceManagementClient
from azure.mgmt.containerinstance import ContainerInstanceManagementClient
from azure.mgmt.containerinstance.models import (ContainerGroup,
                                                 Container,
                                                 EnvironmentVariable,
                                                 ResourceRequests,
                                                 ResourceRequirements,
                                                 OperatingSystemTypes, ImageRegistryCredential,
                                                 VolumeMount, Volume, AzureFileVolume)

# Main API application

app = FastAPI()

SUBSCRIPTION_ID= os.environ.get('SUBSCRIPTION_ID')
REGISTRY_TOKEN= os.environ.get('REGISTRY_TOKEN')
FILESHARE_KEY= os.environ.get('FILESHARE_KEY')

async def get_form_data(request: Request):
    form = await request.form()
    data = form['data'].read()
    mapping = form['mapping'].read()
    data_filename_extension = Path(form["data"].filename).suffix
    return await data, await mapping, data_filename_extension


@app.post("/jobs")
def start_job(files: (bytes, bytes, str) = Depends(get_form_data)):
    data, mapping, data_filename_extension = files

    job_id = str(uuid.uuid4())

    try:
        # Zip both files into an archive (in-memory, using a BytesIO buffer)

        with zipfile.ZipFile(str(Path('input') / job_id )+'.zip', "w") as archive:
            # Data file in archive is suffixed with.csv or .xlsx depending on input file

            archive.writestr(f"data{data_filename_extension}", data)
            archive.writestr("mapping.yml", mapping)

        start_container(job_id)

    except:
        raise







    return {
        "job_id": job_id
    }


@app.get("/jobs/{job_id}/status")
def get_job_status(job_id: str):
    return {
        "name": 'running',
    }


@app.get("/jobs/{job_id}/result")
def return_result(job_id: str):

    return FileResponse(str(Path('output') / job_id) + '.rds', filename='post.json')

def start_container(job_id):
    # Start container
    #
    # This requires:
    # 1.) System-managed identity to be enabled (your Function app -> Identity -> Set Status to On)
    # 2.) The following role assignment on the Function app -> Identity -> Azure role assignments -> Add role assignment:
    #   - Your resource group, Role: Reader
    # 3.) On your ACI container group: Access Control (IAM) -> Add role assignment -> Contributor -> Next -> Assign access to Managed identity -> Select members -> All system-assigned managed identities -> your functions app
    #
    # Downside of 3.) is that we can only limit it to specific container groups, which would limit concurrency (only one container per group)
    # Instead, instead of assigning the Reader role to the resource group in the Function app, assign Contributor. For more specific roles, we need access to the AD
    # Also see: https://learn.microsoft.com/en-us/samples/azure-samples/functions-storage-managed-identity/using-managed-identity-between-azure-functions-and-azure-storage/

    container_resource_requests = ResourceRequests(memory_in_gb=4, cpu=1.0)
    logging.info(container_resource_requests)
    container_resource_requirements = ResourceRequirements(requests=container_resource_requests)
    logging.info(container_resource_requirements)

    credential = DefaultAzureCredential()
    logging.info(credential)
    print(credential)
    resource_client = ResourceManagementClient(credential, SUBSCRIPTION_ID)
    resource_group = resource_client.resource_groups.get("Summer-fair")
    logging.info(resource_group)

    # credential = DefaultAzureCredential()
    # subscription_client = SubscriptionClient(credential)
    #
    # wrapped_credential = AzureIdentityCredentialAdapter(credential)
    # policy_client = PolicyInsightsClient(credentials=wrapped_credential)

    container = Container(
        name="pipeline",
        image="pipelinesummerfair.azurecr.io/pipeline",
        resources=container_resource_requirements,
        environment_variables=[
            EnvironmentVariable(name="ADMIN_PASSWORD", value="admin")
        ],
        volume_mounts=[VolumeMount(name='datafileshare', mount_path='/data')],
        command=['ls', '/data/']
    )
    logging.info(container)
    group = ContainerGroup(
        location=resource_group.location,
        containers=[container],
        os_type=OperatingSystemTypes.linux,
        image_registry_credentials=[
            ImageRegistryCredential(
                server="pipelinesummerfair.azurecr.io",
                username="pipelineSummerfair",
                password= REGISTRY_TOKEN
            )

        ],

        volumes=[Volume(name='datafileshare', azure_file=AzureFileVolume(share_name='fileshare-summerfair',
                                                                         storage_account_name='summerfair9d8d',
                                                                         storage_account_key=FILESHARE_KEY))]
    )
    logging.info(group)
    aci_client = ContainerInstanceManagementClient(credential, SUBSCRIPTION_ID)
    logging.info(aci_client)

    # container_group_name = "test-group-matthijs"
    container_group_name = job_id
    logging.info(aci_client.container_groups.begin_create_or_update(resource_group.name, container_group_name, group))


def main(req: func.HttpRequest, context: func.Context) -> func.HttpResponse:
    """Each request is redirected to the ASGI handler."""


    logging.info(f'Request: {req}')
    logging.info(f'Context: {context}')
    return func.AsgiMiddleware(app).handle(req, context)
