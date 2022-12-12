import logging
import uuid
import zipfile

import azure.functions as func
from fastapi import FastAPI, Request, Depends
from fastapi.responses import FileResponse

from pathlib import Path


# Main API application

app = FastAPI()


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




def main(req: func.HttpRequest, context: func.Context) -> func.HttpResponse:
    """Each request is redirected to the ASGI handler."""

    logging.info(f'Request: {req}')
    logging.info(f'Context: {context}')
    return func.AsgiMiddleware(app).handle(req, context)
